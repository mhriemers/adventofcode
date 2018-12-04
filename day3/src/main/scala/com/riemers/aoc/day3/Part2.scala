package com.riemers.aoc.day3

import cats.effect.ExitCode
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}

object Part2 extends TaskApp with ObservableHelpers {
  override def run(args: List[String]): Task[ExitCode] = for {
    claims ← parseObservable(readFileFromResource("input.txt")).toListL
    c ← func(claims)
    _ ← Task(println(c))
  } yield ExitCode.Success

  /**
    * TODO: Apply a filter since an overlapping between head and tail elements
    * means that these tail elements also can't be non-overlapping themselves.
    *
    * @param claims A list of claims of fabric
    * @return
    */
  def func(claims: List[Claim]): Task[Option[Claim]] = Task {
    (claims map { c1 ⇒
      if (claims.filterNot(_.equals(c1)).forall(_.nonOverlap(c1))) Some(c1)
      else None
    } filter (_.isDefined) map (_.get)).headOption
  }
}
