package com.riemers.aoc.day3

import cats.effect.ExitCode
import com.riemers.aoc.common.{ObservableHelpers, putStrLn}
import monix.eval.{Task, TaskApp}

object Part1 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] = {
    for {
      claims ← parseObservable(readFileFromResource("input.txt")).toListL
      points = claimsToPoints(claims)
      l = func(points)
      _ ← putStrLn(l)
    } yield ExitCode.Success
  }

  def func(points: List[Point]): Long = {
    points groupBy (p ⇒ (p.x, p.y)) count (_._2.lengthCompare(1) > 0)
  }

}
