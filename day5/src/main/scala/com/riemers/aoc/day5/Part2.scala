package com.riemers.aoc.day5

import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

object Part2 extends TaskApp {
  override def run(args: List[String]): Task[ExitCode] = for {
    chars ← readFileFromResourceAsChars("input.txt")
      .flatMap(array ⇒ Observable(array: _*))
      .toListL
    perms = permutations(chars)
    tasks ← Task.traverse(perms.map(collapse))(Task.fromEval)
    length = tasks.map(_.length).min
    _ ← Task(println(length))
  } yield ExitCode.Success

  def permutations(chars: List[Char]): List[List[Char]] = {
    Range.inclusive('a', 'z').map(_.toChar).map { c1 ⇒
      filterChar(chars, c1)
    }.toList
  }

  def filterChar(chars: List[Char], c1: Char): List[Char] = {
    chars.filterNot(c2 ⇒ c1 == Character.toLowerCase(c2))
  }
}
