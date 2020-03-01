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

    "tail should return List(2, 3)" in {
      List.tail(List(1, 2, 3)) shouldBe List(2, 3)
    }

    "tail should return List() if tail is empty" in {
      List.tail(List(1)) shouldBe List()
    }

    "an exception is raised if an empty list is passed through" in {
      intercept[Exception] { List.tail(List()) }

      // alternative below

      val message = intercept[Exception] { List.tail(List()) }
      assert(message.getMessage === "no tail on Nil")

      // another alternative below

      message.getMessage shouldBe "no tail on Nil"

    }

  }

}
