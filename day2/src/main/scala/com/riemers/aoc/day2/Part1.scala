package com.riemers.aoc.day2

import cats.effect.ExitCode
import cats.{Applicative, Defer}
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}

import scala.language.higherKinds

object Part1 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] = for {
    list ← readFileFromResource("input.txt").toListL
    long ← func[Task](list)
    _ ← Task(println(long))
  } yield ExitCode.Success

  def func[F[_] : Defer : Applicative](strings: List[String], two: Long = 0, three: Long = 0): F[Long] = {
    strings match {
      case Nil ⇒ Applicative[F].pure(two * three)
      case head :: tail ⇒
        val map = head.toCharArray.groupBy(identity).map {
          case (char, array) ⇒ array.length -> char
        }
        Defer[F].defer(func[F](
          tail,
          map.get(2).fold(two)(_ + 1),
          map.get(3).fold(three)(_ + 1)
        ))
    }
  }

}
