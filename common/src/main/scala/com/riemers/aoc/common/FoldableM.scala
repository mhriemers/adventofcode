package com.riemers.aoc.common

import cats.{Applicative, Foldable, Id}
import monix.eval.TaskLift
import monix.reactive.Observable

import scala.language.higherKinds

trait FoldableM[F[_], G[_]] {

  def foldLeftM[A, B](fa: F[A], z: B)(f: (B, A) => B): G[B]

}

object FoldableM extends FoldableMInstances0 {

  def apply[F[_], G[_]](implicit instance: FoldableM[F, G]): FoldableM[F, G] = instance

  implicit def foldableMObservableTaskLike[G[_] : TaskLift]: FoldableM[Observable, G] = new FoldableM[Observable, G] {
    override def foldLeftM[A, B](fa: Observable[A], z: B)(f: (B, A) ⇒ B): G[B] =
      fa.foldLeftL(z)(f).to[G]
  }

  implicit val foldableMObservable: FoldableM[Observable, Observable] = new FoldableM[Observable, Observable] {
    override def foldLeftM[A, B](fa: Observable[A], z: B)(f: (B, A) ⇒ B): Observable[B] =
      fa.foldLeft(z)(f)
  }

  implicit def foldableMfoldableId[F[_]](implicit F: Foldable[F]): FoldableM[F, Id] = new FoldableM[F, Id] {
    override def foldLeftM[A, B](fa: F[A], z: B)(f: (B, A) ⇒ B): Id[B] =
      F.foldLeft(fa, z)(f)
  }

}

trait FoldableMInstances0 {

  implicit def foldableMfoldable[F[_], G[_]](implicit F: Foldable[F], A: Applicative[G]): FoldableM[F, G] = new FoldableM[F, G] {
    override def foldLeftM[A, B](fa: F[A], z: B)(f: (B, A) ⇒ B): G[B] =
      A.pure(F.foldLeft(fa, z)(f))
  }

}