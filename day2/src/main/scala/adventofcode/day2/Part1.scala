package adventofcode.day2

import adventofcode.common.{ObservableHelpers, console}
import cats.effect.ExitCode
import cats.instances.long._
import cats.{Applicative, Defer}
import monix.eval.{Task, TaskApp}

import scala.language.higherKinds

object Part1 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] = for {
    list ← readFileFromResource("input.txt").toListL
    long ← func[Task](list)
    _ ← console.putStrLn(long)
  } yield ExitCode.Success

  def func[F[_]](input: List[String])(implicit A: Applicative[F], D: Defer[F]): F[Long] = {
    def rec(strings: List[String], two: Long, three: Long): F[Long] = {
      strings match {
        case Nil ⇒ A.pure(two * three)
        case head :: tail ⇒
          val map = head.groupBy(identity).map {
            case (char, array) ⇒ array.length -> char
          }
          D.defer(rec(
            tail,
            map.get(2).fold(two)(_ + 1),
            map.get(3).fold(three)(_ + 1)
          ))
      }
    }

    D.defer(rec(input, 0, 0))
  }

}
