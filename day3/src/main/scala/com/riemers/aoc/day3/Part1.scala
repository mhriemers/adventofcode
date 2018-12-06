package com.riemers.aoc.day3

import cats.effect.ExitCode
import com.riemers.aoc.common.{ObservableHelpers, putStrLn}
import monix.eval.{Task, TaskApp}

import scala.language.postfixOps

object Part1 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] = {
    for {
      squares ← claimsToSquares(
        parseCollection(readFileFromResource("input.txt"))
      ).toListL
      l = func(squares)
      _ ← putStrLn(l)
    } yield ExitCode.Success
  }

  def func(points: List[Square]): Long =
    (points groupBy (p ⇒ (p.x, p.y)) values) count (_.lengthCompare(1) > 0)

}
