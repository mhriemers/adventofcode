package adventofcode.day5

import adventofcode.common.{FromRange, ObservableHelpers, console}
import cats.effect.ExitCode
import cats.instances.int._
import cats.instances.option._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.{Functor, FunctorFilter}
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

import scala.language.higherKinds

object Part2 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] =
    permutations[Observable](readFileFromResourceAsChars("input.txt")
      .flatMap(array ⇒ Observable(array: _*))
    )
      .mapEval(_.toListL)
      .mapEvalF(collapse)
      .map(_.length)
      .minL
      .flatMap(console.putStrLn[Task, Option[Int]])
      .map(_ ⇒ ExitCode.Success)

  def permutations[F[_] : Functor : FunctorFilter](chars: F[Char])(implicit R: FromRange[F]): F[F[Char]] = {
    R.rangeInclusive('a', 'z')
      .map(_.toChar)
      .map(c ⇒ chars.filter(Character.toLowerCase _ andThen (_ != c)))
  }
}
