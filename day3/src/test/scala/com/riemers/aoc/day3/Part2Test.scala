package com.riemers.aoc.day3

import monix.execution.schedulers.TestScheduler
import org.scalatest.{FunSuite, Matchers}

class Part2Test extends FunSuite with Matchers {

  implicit val ec: TestScheduler = TestScheduler()

  test("it should pass the basic test case") {
    val c1 = Claim(1, 1, 3, 4, 4)
    val c2 = Claim(2, 3, 1, 4, 4)
    val c3 = Claim(3, 5, 5, 2, 2)

    val claims = c1 :: c2 :: c3 :: Nil
    val option = Part2.func(claimsToPoints(claims), claims).runSyncUnsafe()
    option shouldBe Some(c3)
  }

}
