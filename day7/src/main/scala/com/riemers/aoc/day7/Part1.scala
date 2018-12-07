package com.riemers.aoc.day7

import cats.effect.ExitCode
import com.riemers.aoc.common.{ObservableHelpers, console}
import monix.eval.{Task, TaskApp}

import scala.language.higherKinds

object Part1 extends TaskApp with ObservableHelpers {

  def reduce(steps: List[(Char, Char)]): List[Char] = {
    steps match {
      case l@Nil ⇒ l
      case (one, two) :: Nil ⇒ one :: two :: Nil
      case l ⇒
        val keys = l.map(_._1).toSet
        val values = l.map(_._2).toSet
        val list = keys.diff(values).toList
        list.sorted match {
          case k@Nil ⇒ k
          case head :: _ ⇒ head +: reduce(l.filter {
            case (c1, _) ⇒ c1 != head
          })
        }
    }
  }

  override def run(args: List[String]): Task[ExitCode] = for {
    lines ← parseF(readFileFromResource("input.txt")).toListL
    result = reduce(lines).mkString
    _ ← console.putStrLn(result)
  } yield ExitCode.Success
}
