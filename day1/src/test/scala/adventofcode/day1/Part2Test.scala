package adventofcode.day1

import cats.Eval
import org.scalatest.{FunSuite, Matchers}

class Part2Test extends FunSuite with Matchers {

  test("it passes the simple test cases") {
    val cases = ("+1, -1" → 0) ::
      ("+3, +3, +4, -2, -4" → 10) ::
      ("-6, +3, +8, +5, -6" → 5) ::
      ("+7, +7, -2, -7, -4" → 14) :: Nil

    cases foreach {
      case (testcase, result) ⇒
        Part2.func[Eval](testcase.split(", ").toList).value shouldBe result
    }
  }

}
