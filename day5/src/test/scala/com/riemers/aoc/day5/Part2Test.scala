package com.riemers.aoc.day5

import cats.instances.list._
import org.scalatest.{FunSuite, Matchers}

class Part2Test extends FunSuite with Matchers {

  test("it should pass the basic test case") {
    val input = "dabAcCaCBAcCcaDA".toList
    val list: List[List[Char]] = Part2.permutations[List](input)

    list should contain("dbcCCBcCcD".toCharArray.toList)
    list should contain("daAcCaCAcCcaDA".toCharArray.toList)
    list should contain("dabAaBAaDA".toCharArray.toList)
    list should contain("abAcCaCBAcCcaA".toCharArray.toList)

    list.map(collapse).map(_.value.length).min shouldBe 4
  }

}
