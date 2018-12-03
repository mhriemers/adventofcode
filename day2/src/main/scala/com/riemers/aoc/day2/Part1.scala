package com.riemers.aoc.day2

import cats.effect.ExitCode
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}

object Part1 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] = for {
    file ← readFileFromResource("input.txt")
    list ← fileToList(file)
    long ← func(list)
    _ ← Task(println(long))
  } yield ExitCode.Success

  def func(strings: List[String], two: Long = 0, three: Long = 0): Task[Long] = {
    if (strings.isEmpty) return Task.pure(two * three)
    val map = strings.head.toCharArray.groupBy(identity).mapValues(_.length).map(_.swap)
    Task.defer(func(strings.drop(1), two + map.get(2).fold(0)(_ ⇒ 1), three + map.get(3).fold(0)(_ ⇒ 1)))
  }

}
