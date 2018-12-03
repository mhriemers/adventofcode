package com.riemers.aoc.day3

import monix.execution.schedulers.TestScheduler
import org.scalatest.{FunSuite, Matchers}

class Part2Test extends FunSuite with Matchers {

  implicit val ec: TestScheduler = TestScheduler()

  test("it should pass the basic test case") {
    val c1 = Claim(1, 1, 3, 4, 4)
    val c2 = Claim(2, 3, 1, 4, 4)
    val c3 = Claim(3, 5, 5, 2, 2)

    c1.nonOverlap(c2) shouldBe false
    c1.nonOverlap(c3) shouldBe true
    c2.nonOverlap(c1) shouldBe false
    c2.nonOverlap(c3) shouldBe true
    c3.nonOverlap(c1) shouldBe true
    c3.nonOverlap(c2) shouldBe true

    val option = Part2.func(c1 :: c2 :: c3 :: Nil).runSyncUnsafe()
    option shouldBe Some(c3)
  }

}
