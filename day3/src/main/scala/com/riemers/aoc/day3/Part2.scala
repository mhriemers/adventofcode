package com.riemers.aoc.day3

import cats.effect.ExitCode
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}

object Part2 extends TaskApp with ObservableHelpers {
  override def run(args: List[String]): Task[ExitCode] = for {
    claims ← parseObservable(readFileFromResource("input.txt")).toListL
    c = func(claims)
    _ ← Task(println(c))
  } yield ExitCode.Success

  /**
    * TODO: Rewrite to be more efficient
    *
    * @param claims List of claims
    * @return
    */
  def func(claims: List[Claim]): Option[Long] = {
    claimsToPoints(claims).groupBy(p ⇒ (p.x, p.y)).filter {
      case ((_, _), list) ⇒ list.lengthCompare(1) == 0
    }.values.flatten.groupBy(_.id).find {
      case (id, p) ⇒
        val value = claims.find(_.id == id).get
        p.size == value.width * value.height
    }.map(_._1)
  }
}
