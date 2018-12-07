package com.riemers.aoc.day1

import cats.instances.list._
import org.scalatest.{FunSuite, Matchers}

class Part1Test extends FunSuite with Matchers {

  test("it should pass the basic test cases") {
    List("+1, +1, +1" → 3l, "+1, +1, -2" → 0l, "-1, -2, -3" → -6l).map {
      case (s, i) ⇒ Part1.countFrequency(s.split(", ").toList) -> i
    }.foreach {
      case (l, i) ⇒ l shouldBe i
    }
  }

}
