package adventofcode.day5

import org.scalatest.{FunSuite, Matchers}
import cats.instances.list._

class Part2Test extends FunSuite with Matchers {

  test("it should generate correct permutations") {
    val input = "dabAcCaCBAcCcaDA".toList
    val list: List[List[Char]] = Part2.permutations[List](input)

    list should contain allOf(
      "dbcCCBcCcD".toList,
      "daAcCaCAcCcaDA".toList,
      "dabAaBAaDA".toList,
      "abAcCaCBAcCcaA".toList
    )
  }

  test("it should pass the basic test case") {
    val input = "dabAcCaCBAcCcaDA".toList
    Part2.permutations[List](input)
      .map(collapse)
      .map(_.value.length)
      .reduceOption(Math.min) shouldBe Some(4)
  }

}
