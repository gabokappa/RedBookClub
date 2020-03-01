package chapter3

import chapter3.PatternMatching.List
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PatternMatchingSpec extends AnyFreeSpec with Matchers {

  "List" - {
    "X should return 3" in {
      // another way of asserting
      assertResult(3) { List.x }
      List.x shouldBe 3
      // another way of asserting it isn't something else
      assert(List.x != 4)
    }
  }

}
