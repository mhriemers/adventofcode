package com.riemers.aoc.day5

import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import org.scalatest.{FunSuite, Matchers}

class Part2Test extends FunSuite with Matchers {

  implicit val ctx: TestScheduler = TestScheduler()

  test("it should pass the basic test case") {
    val input = Observable("dabAcCaCBAcCcaDA".toCharArray: _*)
    val list = Part2.permutations(input).mapEval(_.toListL).toListL.runSyncUnsafe()

    list should contain("dbcCCBcCcD".toCharArray.toList)
    list should contain("daAcCaCAcCcaDA".toCharArray.toList)
    list should contain("dabAaBAaDA".toCharArray.toList)
    list should contain("abAcCaCBAcCcaA".toCharArray.toList)

    list.map(collapse).map(_.value.length).min shouldBe 4
  }

}
