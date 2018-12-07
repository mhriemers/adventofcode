package com.riemers.aoc.day6

import cats.data.{NonEmptyList ⇒ Nel}
import cats.effect.ExitCode
import cats.instances.int._
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

    val states = coords.map {
      case c@(x, y) ⇒ c → (points.map(p ⇒ p.id → p.dist(x, y)).sortBy(_._2) match {
        case Nel((i, 0), _) ⇒ Claimed(i)
        case Nel((i, _), Nil) ⇒ Claimed(i)
        case Nel((i, d1), (_, d2) :: _) ⇒ if (d1 == d2) Contested else Claimed(i)
      })
    }

    val filters = states.foldLeft(Set.empty[Int]) {
      case (set, ((x, y), Claimed(i))) if x == xmin || x == xmax || y == ymin || y == ymax ⇒ set + i
      case (set, _) ⇒ set
    }

    states.filterNot {
      case (_, Claimed(i)) ⇒ filters.contains(i)
      case (_, Contested) ⇒ true
    }.map(_._2.asInstanceOf[Claimed]).groupBy(_.id).values.map(_.length).max
  }

  sealed trait State

  case class Claimed(id: Int) extends State

  case object Contested extends State

}
