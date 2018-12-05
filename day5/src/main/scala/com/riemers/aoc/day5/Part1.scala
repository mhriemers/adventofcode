package com.riemers.aoc.day5

import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

object Part1 extends TaskApp {
  override def run(args: List[String]): Task[ExitCode] = for {
    chars ← readFileFromResourceAsChars("input.txt")
      .flatMap(array ⇒ Observable(array: _*))
      .toListL
    string ← Task.fromEval(collapse(chars))
    _ ← Task(println(string.length))
  } yield ExitCode.Success
}
