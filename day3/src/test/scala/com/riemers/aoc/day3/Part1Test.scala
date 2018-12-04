package com.riemers.aoc.day3

import com.riemers.aoc.day3.Part1.{func, readFileFromResource}
import monix.execution.schedulers.TestScheduler
import org.scalatest.{FunSuite, Matchers}

class Part1Test extends FunSuite with Matchers {

  implicit val ec: TestScheduler = TestScheduler()

  test("it passes the test case") {
    val input =
      """#1 @ 1,3: 4x4
        |#2 @ 3,1: 4x4
        |#3 @ 5,5: 2x2""".stripMargin

    val l = (for {
      claims ← parseObservable(readFileFromResource("input.txt")).toListL
    } yield func(claimsToPoints(claims))).runSyncUnsafe()

    l shouldBe 4
  }

}
