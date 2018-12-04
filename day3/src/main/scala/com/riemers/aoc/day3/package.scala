package com.riemers.aoc

import cats.data.OptionT
import monix.eval.Task
import monix.reactive.Observable

import scala.util.Try
import scala.util.matching.Regex

package object day3 {

  val regex: Regex = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r

  def claimsToPoints(claims: List[Claim]): List[Point] = {
    for {
      claim ← claims
      x ← claim.left until claim.left + claim.width
      y ← claim.top until claim.top + claim.height
    } yield Point(claim.id, x, y)
  }

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

  case class Claim(id: Long, left: Long, top: Long, width: Long, height: Long)

  case class Point(id: Long, x: Long, y: Long)

}
