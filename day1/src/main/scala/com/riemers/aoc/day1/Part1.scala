package com.riemers.aoc.day1

import cats.effect._
import com.riemers.aoc.common.{FoldableM, ObservableHelpers, putStrLn}
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

import scala.language.higherKinds

object Part1 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] = {
    val file = readFileFromResource("input.txt")
    for {
      frequency ← countFrequency[Observable, Task](file)
      _ ← putStrLn(frequency)
    } yield ExitCode.Success
  }

  def countFrequency[F[_], G[_]](strings: F[String])(implicit F: FoldableM[F, G]): G[Long] = {
    F.foldLeftM(strings, 0l)((b, string) ⇒ b + frequencyToLong(string))
  }

}
