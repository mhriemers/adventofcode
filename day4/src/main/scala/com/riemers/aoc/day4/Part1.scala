package com.riemers.aoc.day4

import cats.TraverseFilter
import cats.effect.ExitCode
import cats.instances.list._
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}

object Part1 extends TaskApp with ObservableHelpers {
  override def run(args: List[String]): Task[ExitCode] = for {
    list ← readFileFromResource("input.txt")
      .toListL
    records = TraverseFilter[List].mapFilter(list)(parseCrudeRecord)
    sorted = records.sortBy(_.date)
    map ← Task.fromEval(countMinutesAsleep(sorted))
    l = func(map)
    _ ← Task(println(l))
  } yield ExitCode.Success

  def func(map: Map[(Int, Int), Int]): Int = {
    val (guard, _) = map.groupBy(_._1._1).mapValues(_.values.sum).maxBy(_._2)
    val ((_, minute), _) = map.filterKeys(_._1 == guard).maxBy(_._2)
    guard * minute
  }
}
