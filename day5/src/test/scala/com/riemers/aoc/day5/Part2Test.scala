package com.riemers.aoc.day5

import cats.instances.list._
import com.riemers.aoc.day5.Part2.permutations
import org.scalatest.{FunSuite, Matchers}

class Part2Test extends FunSuite with Matchers {

  test("it should generate correct permutations") {
    val input = "dabAcCaCBAcCcaDA".toList
    val list: List[List[Char]] = permutations[List](input)

    list should contain allOf(
      "dbcCCBcCcD".toList,
      "daAcCaCAcCcaDA".toList,
      "dabAaBAaDA".toList,
      "abAcCaCBAcCcaA".toList
    )
  }

  test("it should pass the basic test case") {
    val input = "dabAcCaCBAcCcaDA".toList
    permutations[List](input)
      .map(collapse)
      .map(_.value.length)
      .reduceOption(Math.min) shouldBe Some(4)
  }

}
