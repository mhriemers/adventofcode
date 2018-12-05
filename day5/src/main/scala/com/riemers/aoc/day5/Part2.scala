package com.riemers.aoc.day5

import cats.effect.ExitCode
import cats.instances.int._
import cats.{Applicative, Functor, FunctorFilter, SemigroupK}
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

import scala.language.higherKinds

object Part2 extends TaskApp {

  /**
    * Temporary polyfill
    */
  implicit val observableFunctorFilter: FunctorFilter[Observable] = new FunctorFilter[Observable] {
    override def functor: Functor[Observable] = Observable.catsInstances

    override def mapFilter[A, B](fa: Observable[A])(f: A ⇒ Option[B]): Observable[B] =
      fa.collect(Function.unlift(f))
  }

  override def run(args: List[String]): Task[ExitCode] =
    permutations[Observable](readFileFromResourceAsChars("input.txt")
      .flatMap(array ⇒ Observable(array: _*))
    )
      .mapEval(_.toListL)
      .mapEvalF(collapse)
      .map(_.length)
      .minL
      .flatMap(l ⇒ Task(println(l)))
      .map(_ ⇒ ExitCode.Success)

  def permutations[F[_] : Applicative : FunctorFilter : SemigroupK](chars: F[Char]): F[F[Char]] = {
    Range.inclusive('a', 'z').map(_.toChar)
      .map { c ⇒
        FunctorFilter[F].filter(chars)(Character.toLowerCase _ andThen (_ != c))
      }
      .map(Applicative[F].pure)
      .reduceLeft(SemigroupK[F].combineK)
  }
}
