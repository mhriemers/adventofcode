package com.riemers.aoc.day3

import cats.FunctorFilter
import cats.instances.list._
import org.scalatest.{FunSuite, Matchers}

class Part1Test extends FunSuite with Matchers {

  test("it passes the test case") {
    val input =
      """#1 @ 1,3: 4x4
        |#2 @ 3,1: 4x4
        |#3 @ 5,5: 2x2""".stripMargin

    val claims = FunctorFilter[List].mapFilter(input.split(System.lineSeparator()).toList)(parse)
    val l = Part1.func(claimsToSquares(claims))

    l shouldBe 4
  }

}
