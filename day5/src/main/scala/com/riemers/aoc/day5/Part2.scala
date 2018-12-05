package com.riemers.aoc.day5

import cats.effect.ExitCode
import cats.instances.int._
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

object Part2 extends TaskApp {
  override def run(args: List[String]): Task[ExitCode] =
    permutations(readFileFromResourceAsChars("input.txt")
      .flatMap(array ⇒ Observable(array: _*))
    )
      .mapEval(_.toListL)
      .mapEvalF(collapse)
      .map(_.length)
      .minL
      .flatMap(l ⇒ Task(println(l)))
      .map(_ ⇒ ExitCode.Success)

  def permutations(chars: Observable[Char]): Observable[Observable[Char]] = {
    Observable.range('a', 'z').map(_.toChar).map { c1 ⇒
      chars.filter(Character.toLowerCase _ andThen (_ != c1))
    }
  }
}
