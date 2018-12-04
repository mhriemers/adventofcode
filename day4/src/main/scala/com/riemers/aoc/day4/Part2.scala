package com.riemers.aoc.day4

import cats.effect.ExitCode
import com.riemers.aoc.day4.Part1.readFileFromResource
import monix.eval.{Task, TaskApp}

object Part2 extends TaskApp {
  override def run(args: List[String]): Task[ExitCode] = for {
    filtered ← readFileFromResource("input.txt")
      .mapEval(parseCrudeRecord)
      .filter(_.isDefined)
      .map(_.get)
      .toListL
    sorted ← Task(filtered.sortBy(_.date))
    map ← countMinutesAsleep(sorted)
    tuple ← Task(map.maxBy(_._2)._1)
    (guard, minute) = tuple
    _ ← Task(println(guard * minute))
  } yield ExitCode.Success
}
