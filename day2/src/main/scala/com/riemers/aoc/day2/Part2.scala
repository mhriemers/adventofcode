package com.riemers.aoc.day2

import cats.data.OptionT
import cats.effect.ExitCode
import cats.instances.list._
import cats.{Applicative, Monad, TraverseFilter}
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}

import scala.language.higherKinds

object Part2 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] = for {
    list ← readFileFromResource("input.txt").toListL
    opt ← func[Task](list).value
    _ ← Task(println(opt))
  } yield ExitCode.Success

  def func[F[_] : Monad](list: List[String]): OptionT[F, String] = {
    list match {
      case Nil ⇒ OptionT.none[F, String]
      case head :: tail ⇒
        OptionT(
          Applicative[F].pure(
            tail.map { str ⇒
              intersection(head, str)
            }.find(_.length + 1 == head.length)
          )
        ).orElse(func[F](tail))
    }
  }

  def intersection(s1: String, s2: String): String = {
    val tuples: List[(Char, Char)] = (s1 zip s2).toList
    TraverseFilter[List].mapFilter(tuples) {
      case (c1, c2) ⇒ if (c1 == c2) Some(c1) else None
    }.mkString
  }

}
