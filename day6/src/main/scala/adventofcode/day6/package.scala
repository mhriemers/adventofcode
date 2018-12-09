package adventofcode

import adventofcode.common.ZipWithIndex
import adventofcode.common.ZipWithIndex.ops._
import cats.FunctorFilter
import cats.data.{NonEmptyList ⇒ Nel}
import cats.syntax.functorFilter._

import scala.language.{higherKinds, postfixOps}
import scala.util.Try

package object day6 {

  def generateCoordinates(xmin: Int, xmax: Int, ymin: Int, ymax: Int): List[(Int, Int)] = {
    (xmin to xmax).flatMap { x ⇒
      (ymin to ymax).map(x → _)
    }.toList
  }

  def findExtremes(points: Nel[Point]): (Int, Int, Int, Int) = {
    points match {
      case Nel(Point(_, x, y), tail) ⇒
        tail.foldLeft((x, x, y, y)) {
          case ((xi, xa, yi, ya), Point(_, x1, y1)) ⇒
            (Math.min(xi, x1), Math.max(xa, x1), Math.min(yi, y1), Math.max(ya, y1))
        }
    }
  }

  def parseF[F[_] : FunctorFilter : ZipWithIndex](input: F[String]): F[Point] = {
    input.zipWithIndex.mapFilter(parse _ tupled)
  }

  def parse(string: String, index: Long): Option[Point] = {
    string.split(", ").toList match {
      case head :: tail :: Nil ⇒ (for {
        x ← Try(head.toInt)
        y ← Try(tail.toInt)
      } yield Point(index.toInt, x, y)).toOption
      case _ ⇒ None
    }
  }

  case class Point(id: Int, x: Int, y: Int) {
    def dist(x1: Int, y1: Int): Int = Math.abs(x - x1) + Math.abs(y - y1)
  }

}
