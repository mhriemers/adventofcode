package com.riemers.aoc.day4

import cats.effect.ExitCode
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}

object Part1 extends TaskApp with ObservableHelpers {
  override def run(args: List[String]): Task[ExitCode] = for {
    filtered ← readFileFromResource("input.txt")
      .mapEval(parseCrudeRecord)
      .filter(_.isDefined)
      .map(_.get)
      .toListL
    sorted ← Task(filtered.sortBy(_.date))
    map ← countMinutesAsleep(sorted)
    t1 ← Task(map.groupBy(_._1._1).mapValues(_.values.sum).maxBy(_._2))
    (guard, _) = t1
    t2 ← Task(map.filterKeys(_._1 == guard).maxBy(_._2))
    ((_, minute), _) = t2
    _ ← Task(println(guard * minute))
  } yield ExitCode.Success
}
