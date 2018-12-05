package com.riemers.aoc.day2

import cats.Id
import org.scalatest.{FunSuite, Matchers}

class Part2Test extends FunSuite with Matchers {

  test("it handles the test case correctly") {
    val strings = "abcde" :: "fghij" :: "klmno" :: "pqrst" :: "fguij" :: "axcye" :: "wvxyz" :: Nil
    val option = Part2.func[Id](strings).value
    option shouldBe Some("fgij")
  }

}
