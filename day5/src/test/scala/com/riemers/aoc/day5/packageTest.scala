package com.riemers.aoc.day5

import org.scalatest.{FunSuite, Matchers}

class packageTest extends FunSuite with Matchers {

  test("it correctly handles edge cases") {
    collapse('a' :: 'A' :: Nil).value shouldBe ""
  }

  test("it should handle the basic test case") {
    List("dabAcCaCBAcCcaDA", "dabAaCBAcCcaDA", "dabCBAcCcaDA")
      .map(_.toList)
      .map(collapse)
      .map(_.value)
      .foreach(_ shouldBe "dabCBAcaDA".toList)
  }

}
