package com.riemers.aoc.day1

import cats.instances.list._
import org.scalatest.{FunSuite, Matchers}

class Part1Test extends FunSuite with Matchers {

  test("it should pass the basic test cases") {
    Part1.countFrequency("+1, +1, +1".split(", ").toList) shouldBe 3
    Part1.countFrequency("+1, +1, -2".split(", ").toList) shouldBe 0
    Part1.countFrequency("-1, -2, -3".split(", ").toList) shouldBe -6
  }

}
