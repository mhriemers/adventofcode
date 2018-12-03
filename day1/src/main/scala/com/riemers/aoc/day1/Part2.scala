package com.riemers.aoc.day1

import cats.effect.ExitCode
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}

object Part2 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] = for {
    file ← readFileFromResource("input.txt")
    frequencies ← fileToList(file)
    long ← func(frequencies)
    _ ← Task(println(long))
  } yield ExitCode.Success

  def func(strings: List[String]): Task[Long] = {
    //noinspection NoTailRecursionAnnotation
    def rec(curr: List[String], freqs: List[Long] = 0 :: Nil): Task[Long] = {
      if (curr.isEmpty) return Task.defer(rec(strings, freqs))
      val l = freqs.last + frequencyToLong(curr.head)
      if (freqs.contains(l)) Task.pure(l)
      else Task.defer(rec(curr.drop(1), freqs :+ l))
    }

    Task.defer(rec(strings))
  }

}

