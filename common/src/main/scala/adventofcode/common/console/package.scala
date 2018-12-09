package adventofcode.common

import cats.Show
import cats.effect.Sync

import scala.language.higherKinds

package object console {

  def putStrLn[F[_], A](a: A)(implicit S: Sync[F], SS: Show[A]): F[Unit] =
    S.delay(println(SS.show(a)))

}
