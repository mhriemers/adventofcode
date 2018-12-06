package com.riemers.aoc.common

import cats.FlatMap
import cats.effect._

import scala.language.higherKinds

abstract class Day[R, S[_]](implicit FIS: FromInputStream[IO, S], F: FlatMap[S]) extends IOApp with ObservableHelpers {
  override def run(args: List[String]): IO[ExitCode] = {
    val file = FIS.fromInputStream(IO(getClass.getClassLoader.getResourceAsStream("input.txt")))

    for {
      result ← Sync[IO].suspend(logic[IO](file))
      _ ← putStrLn[IO](result)
    } yield ExitCode.Success
  }

  def logic[F[_] : Sync](input: S[Byte]): F[R]
}
