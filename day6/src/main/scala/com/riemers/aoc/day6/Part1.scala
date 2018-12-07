package com.riemers.aoc.day6

import cats.Alternative
import cats.data.{NonEmptyList ⇒ Nel}
import cats.effect.ExitCode
import cats.instances.int._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import com.riemers.aoc.common._
import monix.eval.{Task, TaskApp}

import scala.language.{higherKinds, postfixOps}
import scala.util.matching.Regex

object Part1 extends TaskApp with ObservableHelpers {

  val regex: Regex = "(\\d+), (\\d+)".r

  override def run(args: List[String]): Task[ExitCode] = for {
    points ← parseF(readFileFromResource("input.txt")).toListL
    result = func(Nel.fromListUnsafe(points))
    _ ← putStrLn(result)
  } yield ExitCode.Success

  def func(points: Nel[Point]): Long = {
    val (xmin, xmax, ymin, ymax) = findExtremes(points)
    val coords = generateCoordinates(xmin, xmax, ymin, ymax)

    val states = coords.mapFilter { c ⇒
      points.map(p ⇒ p.id → (p.dist _).tupled(c)).sortBy(_._2) match {
        case Nel((i, d1), (_, d2) :: _) ⇒ if (d1 == d2) None else Some(c → i)
        case Nel((i, _), _) ⇒ Some(c → i)
      }
    }

    val filters = states.foldLeft(Set.empty[Int]) {
      case (set, ((x, y), i)) if x == xmin || x == xmax || y == ymin || y == ymax ⇒ set + i
      case (set, _) ⇒ set
    }

    states.mapFilter {
      case (_, i) ⇒ Alternative[Option].guard(!filters.contains(i)).as(i)
      case _ ⇒ None
    }.groupBy(identity).values.map(_.length).max
  }

}
