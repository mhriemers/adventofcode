package com.riemers.aoc

import cats.FunctorFilter
import cats.syntax.functorFilter._

import scala.language.higherKinds
import scala.util.matching.Regex

package object day7 {

  val regex: Regex = "Step (\\w) must be finished before step (\\w) can begin.".r

  def parseF[F[_] : FunctorFilter](lines: F[String]): F[(Char, Char)] = {
    lines.mapFilter(parse)
  }

  def parse(line: String): Option[(Char, Char)] = line match {
    case regex(rc1, rc2) ⇒ for {
      c1 ← rc1.headOption
      c2 ← rc2.headOption
    } yield (c1, c2)
  }

}
