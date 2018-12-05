package com.riemers.aoc.day4

import cats.TraverseFilter
import cats.effect.ExitCode
import cats.instances.list._
import com.riemers.aoc.day4.Part1.readFileFromResource
import monix.eval.{Task, TaskApp}

object Part2 extends TaskApp {
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
    val (guard, minute) = map.maxBy(_._2)._1
    guard * minute
  }
}
