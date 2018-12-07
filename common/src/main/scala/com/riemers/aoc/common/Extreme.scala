package com.riemers.aoc.common

import cats.{Foldable, Order}
import simulacrum.typeclass
import cats.syntax.foldable._
import monix.reactive.Observable

import scala.language.higherKinds

@typeclass trait Extreme[F[_]] {

  def maxByOption[A, B](fa: F[A])(f: A ⇒ B)(implicit O: Order[B]): Option[A]

  def minByOption[A, B](fa: F[A])(f: A ⇒ B)(implicit O: Order[B]): Option[A]

}

object Extreme {

  implicit val extremeObservable: Extreme[Observable] = new Extreme[Observable] {
    override def maxByOption[A, B](fa: Observable[A])(f: A ⇒ B)(implicit O: Order[B]): Option[A] = ???

    override def minByOption[A, B](fa: Observable[A])(f: A ⇒ B)(implicit O: Order[B]): Option[A] = ???
  }

}

trait ExtremeInstances0 {

  implicit def extremeFoldable[F[_] : Foldable]: Extreme[F] = new Extreme[F] {
    override def maxByOption[A, B](fa: F[A])(f: A ⇒ B)(implicit O: Order[B]): Option[A] =
      fa.reduceLeftOption {
        case (x, y) ⇒ if (O.gteqv(f(x), f(y))) x else y
      }

    override def minByOption[A, B](fa: F[A])(f: A ⇒ B)(implicit O: Order[B]): Option[A] =
      fa.reduceLeftOption {
        case (x, y) ⇒ if (O.lteqv(f(x), f(y))) x else y
      }
  }

}
