package com.riemers.aoc.day1

import cats.effect._
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

object Part1 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] =
    for {
      file ← readFileFromResource("input.txt")
      frequency ← countFrequency(file)
      _ ← Task(println(frequency))
    } yield ExitCode.Success

  def countFrequency(strings: Observable[String]): Task[Long] =
    strings.foldLeftL(0l)((b, string) ⇒ b + frequencyToLong(string))

}
