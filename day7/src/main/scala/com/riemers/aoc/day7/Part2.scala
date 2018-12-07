package com.riemers.aoc.day7

import cats.effect.ExitCode
import com.riemers.aoc.common.{ObservableHelpers, console}
import monix.eval.{Task, TaskApp}
import cats.instances.long._

object Part2 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] = for {
    lines ← parseF(readFileFromResource("input.txt")).toListL
    result = reduce(lines, char ⇒ 61 + (char - 'A'), 5)
    _ ← console.putStrLn(result)
  } yield ExitCode.Success

  def reduce(input: List[(Char, Char)], charTime: Char ⇒ Long, concurrent: Int): Long = {
    @scala.annotation.tailrec
    def rec(steps: List[(Char, Char)], solving: List[(Long, Char)], time: Long): Long = {
      steps match {
        case Nil ⇒ time
        case list ⇒
          val keys = list.map(_._1).toSet
          val values = list.map(_._2).toSet
          val diffs = keys.diff(values).toList
            .filter(c ⇒ !solving.map(_._2).contains(c))
          val times = diffs.map(c ⇒ (time + charTime(c)) → c)

          val neew = (solving ++ times.sortBy(_._1).take(concurrent - solving.length)).sortBy(_._1)
          neew match {
            case Nil ⇒
              println("lower")
              println(steps)
              println(solving)
              time
            case (t, c) :: tail ⇒
              val tuples = list.filter {
                case (c1, _) ⇒ c1 != c
              }
              if (tuples.nonEmpty) rec(tuples, tail, t)
              else t + list.map(_._2).map(charTime).max
          }
      }
    }

    rec(input, List.empty, 0)
  }
}
