package com.riemers.aoc.day5

import org.scalatest.{FunSuite, Matchers}

class packageTest extends FunSuite with Matchers {

  test("it correctly handles edge cases") {
    collapse('a' :: 'A' :: Nil).value shouldBe ""
  }

  test("it should handle the basic test case") {
    collapse("dabAcCaCBAcCcaDA".toCharArray.toList).value shouldBe "dabCBAcaDA"
    collapse("dabAaCBAcCcaDA".toCharArray.toList).value shouldBe "dabCBAcaDA"
    collapse("dabCBAcCcaDA".toCharArray.toList).value shouldBe "dabCBAcaDA"
  }

}
