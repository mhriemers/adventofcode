package com.riemers.aoc

import cats.data.OptionT
import monix.eval.Task
import monix.reactive.Observable

import scala.util.Try
import scala.util.matching.Regex

package object day3 {

  val regex: Regex = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r

  def parseObservable(observable: Observable[String]): Observable[Claim] = {
    observable.mapEval(string ⇒ parse(string).value).filter(_.isDefined).map(_.get)
  }

  def parse(string: String): OptionT[Task, Claim] = OptionT(Task {
    string match {
      case regex(sid, sleft, stop, swidth, sheight) ⇒ (for {
        id ← Try(sid.toLong)
        left ← Try(sleft.toLong)
        top ← Try(stop.toLong)
        width ← Try(swidth.toLong)
        height ← Try(sheight.toLong)
      } yield Claim(id, left, top, width, height)).toOption
      case _ ⇒ None
    }
  })

  case class Claim(id: Long, left: Long, top: Long, width: Long, height: Long) {

    def overlap(c: Claim): Boolean = !nonOverlap(c)

    def nonOverlap(c: Claim): Boolean = {
      val (x1, y1) = (left, top)
      val (x2, y2) = (left + (width - 1), top + (height - 1))
      val (x3, y3) = (c.left, c.top)
      val (x4, y4) = (c.left + (c.width - 1), c.top + (c.height - 1))

      x1 > x4 || x3 > x2 || y1 > y4 || y3 > y2
    }

    def overlapping(c: Claim): Option[Long] = {
      val (x1, y1) = (left, top)
      val (x2, y2) = (left + (width - 1), top + (height - 1))
      val (x3, y3) = (c.left, c.top)
      val (x4, y4) = (c.left + (c.width - 1), c.top + (c.height - 1))

      if (!(x1 > x4 || x3 > x2 || y1 > y4 || y3 > y2)) {
        val x = Math.min(x2, x4) - Math.max(x1, x3) + 1
        val y = Math.min(y2, y4) - Math.max(y1, y3) + 1
        Some(x * y)
      } else None
    }
  }

  case class Point(x: Long, y: Long)

}
