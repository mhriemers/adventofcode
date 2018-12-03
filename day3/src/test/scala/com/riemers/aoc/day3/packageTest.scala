package com.riemers.aoc.day3

import org.scalatest.{FunSuite, Matchers}

class packageTest extends FunSuite with Matchers {

  private val c1 = Claim(1, 1, 3, 4, 4)
  test("it correctly identifies overlap") {
    val c2 = Claim(2, 3, 1, 4, 4)
    c1.overlap(c2) shouldBe true
    c1.overlapping(c2) shouldBe Some(4)
  }

  test("it correctly identifies non overlap") {
    val c3 = Claim(3, 5, 5, 2, 2)
    c1.overlap(c3) shouldBe false
    c1.overlapping(c3) shouldBe None
  }

}
