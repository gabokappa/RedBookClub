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

    ".tail should return List(2, 3)" in {
      List.tail(List(1, 2, 3)) shouldBe List(2, 3)
    }

    ".tail should return List() if tail is empty" in {
      List.tail(List(1)) shouldBe List()
    }

    ".tail raises an exception if an empty list is passed through" in {
      intercept[Exception] { List.tail(List()) }

      // alternative below

      val message = intercept[Exception] { List.tail(List()) }
      assert(message.getMessage === "no tail on Nil")

      // another alternative below

      message.getMessage shouldBe "no tail on Nil"

    }

    // SETHEAD

    ".setHead should return List(2, 2, 3)" in {
      List.setHead(List(1, 2, 3), 2) shouldBe List(2, 2, 3)
    }

    ".setHead should return List(bear, bird, cheetah)" in {
      List.setHead(List("ant", "bird", "cheetah"), "bear") shouldBe List("bear", "bird", "cheetah")
    }

    ".setHead should return List(2) if Nil" in {
      List.setHead(List(), 2) shouldBe List(2)
    }

    // DROP
    ".drop should return List(2, 3) when n = 1 " in {
      List.drop(List(1, 2, 3), 1) shouldBe List(2, 3)
    }

    ".drop should return List(3) when n = 2 " in {
      List.drop(List(1, 2, 3), 2) shouldBe List(3)
    }

    ".drop should return List(d) when n = 3 " in {
      List.drop(List("a", "b", "c", "d"), 3) shouldBe List("d")
    }

    ".drop should return List() if l == List() " in {
      List.drop(List(), 1) shouldBe List()
    }

  }

}
