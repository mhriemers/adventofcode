package com.riemers.aoc.day5

import org.scalatest.{FunSuite, Matchers}

class Part2Test extends FunSuite with Matchers {

  test("it should pass the basic test case") {
    val input = "dabAcCaCBAcCcaDA"
    val list = input.toCharArray.toList

    val c1 = Part2.filterChar(list, 'a')
    c1 shouldBe "dbcCCBcCcD".toCharArray.toList
    val c2 = Part2.filterChar(list, 'b')
    c2 shouldBe "daAcCaCAcCcaDA".toCharArray.toList
    val c3 = Part2.filterChar(list, 'c')
    c3 shouldBe "dabAaBAaDA".toCharArray.toList
    val c4 = Part2.filterChar(list, 'd')
    c4 shouldBe "abAcCaCBAcCcaA".toCharArray.toList

    (c1 :: c2 :: c3 :: c4 :: Nil).map(collapse).map(_.value.length).min shouldBe 4
  }

  test("it should create correct permutations") {
    val input = "dabAcCaCBAcCcaDA".toCharArray.toList
    val list = Part2.permutations(input)

    list should contain("dbcCCBcCcD".toCharArray.toList)
    list should contain("daAcCaCAcCcaDA".toCharArray.toList)
    list should contain("dabAaBAaDA".toCharArray.toList)
    list should contain("abAcCaCBAcCcaA".toCharArray.toList)

    list.map(collapse).map(_.value.length).min shouldBe 4
  }

}
