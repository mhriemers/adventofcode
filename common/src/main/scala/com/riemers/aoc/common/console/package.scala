package com.riemers.aoc.common

import cats.Show
import cats.effect.Sync

package object console {

  def putStrLn[F[_], A](a: A)(implicit S: Sync[F], SS: Show[A]): F[Unit] =
    S.delay(println(SS.show(a)))

  def putStrLn[F[_]](a: String)(implicit S: Sync[F]): F[Unit] =
    S.delay(println(a))

}
