package com.riemers.aoc.day3

import com.riemers.aoc.day3.Part1.{constructBoard, func}
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import org.scalatest.{FunSuite, Matchers}

class Part1Test extends FunSuite with Matchers {

  implicit val ec: TestScheduler = TestScheduler()

  test("it passes the test case") {
    val input =
      """#1 @ 1,3: 4x4
        |#2 @ 3,1: 4x4
        |#3 @ 5,5: 2x2""".stripMargin

    val claims = parseObservable(Observable.fromIterable(input.split(System.lineSeparator())))

    val value = for {
      board ← constructBoard(1100, 1100)
      long ← func(claims, board)
    } yield long

    val l = value.runSyncUnsafe()

    l shouldBe 4
  }

}
