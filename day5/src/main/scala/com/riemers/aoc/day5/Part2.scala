package com.riemers.aoc.day5

import cats.effect.ExitCode
import cats.instances.int._
import cats.syntax.functorFilter._
import cats.syntax.semigroupk._
import cats.{Alternative, FunctorFilter}
import com.riemers.aoc.common.{ObservableHelpers, putStrLn}
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
      .flatMap(putStrLn[Task])
      .map(_ ⇒ ExitCode.Success)

  def permutations[F[_] : FunctorFilter](chars: F[Char])(implicit A: Alternative[F]): F[F[Char]] = {
    Range.inclusive('a', 'z').map(_.toChar)
      .map { c ⇒
        chars.filter(Character.toLowerCase _ andThen (_ != c))
      }
      .map(A.pure)
      .reduceLeft(_ <+> _)
  }
}
