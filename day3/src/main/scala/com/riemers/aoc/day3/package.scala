package com.riemers.aoc

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.{FlatMap, FunctorFilter}
import com.riemers.aoc.common.FromRange

import scala.language.higherKinds
import scala.util.Try
import scala.util.matching.Regex

package object day3 {

  val regex: Regex = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r

  def claimsToSquares[F[_] : FlatMap](claims: F[Claim])(implicit R: FromRange[F]): F[Square] = {
    for {
      claim ← claims
      x ← R.range(claim.left, claim.left + claim.width)
      y ← R.range(claim.top, claim.top + claim.height)
    } yield Square(claim, x, y)
  }

  def parseCollection[F[_] : FunctorFilter](strings: F[String]): F[Claim] = {
    strings.mapFilter(parse)
  }

  def parse(string: String): Option[Claim] = {
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
  }

  case class Claim(id: Long, left: Long, top: Long, width: Long, height: Long)

  case class Square(claim: Claim, x: Long, y: Long)

}
