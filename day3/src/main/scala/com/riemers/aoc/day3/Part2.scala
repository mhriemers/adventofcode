package com.riemers.aoc.day3

import cats.effect.ExitCode
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}

object Part2 extends TaskApp with ObservableHelpers {
  override def run(args: List[String]): Task[ExitCode] = for {
    claims ← parseObservable(readFileFromResource("input.txt")).toListL
    claimOpt ← func(claims)
    _ ← Task(println(claimOpt))
  } yield ExitCode.Success

  def func(claims: List[Claim]): Task[Option[Claim]] = Task {
    (claims map { c1 ⇒
      if (claims.filterNot(_.equals(c1)).forall(_.nonOverlap(c1))) Some(c1)
      else None
    } filter (_.isDefined) map (_.get)).headOption
  }
}
