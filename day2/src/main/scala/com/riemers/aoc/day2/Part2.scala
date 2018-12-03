package com.riemers.aoc.day2

import cats.data.OptionT
import cats.effect.ExitCode
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}

object Part2 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] = for {
    file ← readFileFromResource("input.txt")
    list ← fileToList(file)
    opt ← func(list).value
    _ ← Task(println(opt))
  } yield ExitCode.Success

  def func(list: List[String]): OptionT[Task, String] = {
    if (list.isEmpty) OptionT.none
    else OptionT(
      Task.sequence(list.drop(1).map { str ⇒
        intersection(str, list.head)
      }).map(_.find(_.length + 1 == list.head.length))
    ).orElse(func(list.drop(1)))
  }

  def intersection(s1: String, s2: String): Task[String] = Task {
    val tuples = s1 zip s2
    tuples.filter {
      case (c1, c2) ⇒ c1 == c2
    }.map(_._1).mkString
  }

}
