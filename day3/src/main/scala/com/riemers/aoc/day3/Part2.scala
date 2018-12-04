package com.riemers.aoc.day3

import cats.effect.ExitCode
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}

object Part2 extends TaskApp with ObservableHelpers {
  override def run(args: List[String]): Task[ExitCode] = for {
    claims ← parseObservable(readFileFromResource("input.txt")).toListL
    points = claimsToPoints(claims)
    c ← func(points, claims)
    _ ← Task(println(c))
  } yield ExitCode.Success

  def func(points: List[Point], claims: List[Claim]): Task[Option[Long]] = Task {
    points.groupBy(p ⇒ (p.x, p.y)).filter {
      case ((_, _), list) ⇒ list.lengthCompare(1) == 0
    }.values.flatten.groupBy(_.id).find {
      case (id, p) ⇒
        val value = claims.find(_.id == id).get
        p.size == value.width * value.height
    }.map(_._1)
  }
}
