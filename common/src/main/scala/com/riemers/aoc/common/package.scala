package com.riemers.aoc

import cats.effect.Sync

import scala.language.higherKinds

package object common {

  def putStrLn[F[_]](a: Any)(implicit S: Sync[F]): F[Unit] =
    S.delay(println(a))

}
