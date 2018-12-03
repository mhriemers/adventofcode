package com.riemers.aoc.day2

import monix.execution.schedulers.TestScheduler
import org.scalatest.{FunSuite, Matchers}

class Part2Test extends FunSuite with Matchers {

  implicit val ec: TestScheduler = TestScheduler()

  test("it handles the test case correctly") {
    val strings = "abcde" :: "fghij" :: "klmno" :: "pqrst" :: "fguij" :: "axcye" :: "wvxyz" :: Nil
    val option = Part2.func(strings).value.runSyncUnsafe()
    option shouldBe Some("fgij")
  }

}
