package com.riemers.aoc.day3

import cats.effect.ExitCode
import cats.instances.list._
import com.riemers.aoc.common.{ObservableHelpers, putStrLn}
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

import scala.language.postfixOps

object Part2 extends TaskApp with ObservableHelpers {
  override def run(args: List[String]): Task[ExitCode] = for {
    claims ← parseCollection[Observable](readFileFromResource("input.txt")).toListL
    c = func(claims)
    _ ← putStrLn(c)
  } yield ExitCode.Success

  def func(claims: List[Claim]): Option[Long] = {
    claimsToSquares(claims)
      .groupBy(s ⇒ s.x → s.y)
      .values
      .filter(_.lengthCompare(1) == 0)
      .flatten
      .groupBy(_.claim)
      .find {
        case (Claim(_, _, _, width, height), squares) ⇒ squares.size == width * height
      }
      .map {
        case (Claim(id, _, _, _, _), _) ⇒ id
      }
  }
}
