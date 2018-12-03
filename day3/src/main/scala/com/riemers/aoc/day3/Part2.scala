package com.riemers.aoc.day3

import cats.effect.ExitCode
import com.riemers.aoc.day3.Part1.Board
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

object Part2 extends TaskApp {
  override def run(args: List[String]): Task[ExitCode] = ???

  def func(claims: Observable[Claim], board: Board): Task[Claim] = ???
}
