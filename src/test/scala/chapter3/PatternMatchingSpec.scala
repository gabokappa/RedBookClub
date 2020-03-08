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

    // DROPWHILE

    ".dropWhile should return List(3) when f divisible by 2 " in {
      List.dropWhile(List(2, 3))(x => (x % 2) == 0) shouldBe List(3)
    }

    ".dropWhile should return List(1, 2, 3) when f divisible by 2 " in {
      List.dropWhile(List(1, 2, 3))(x  => (x % 2) == 0) shouldBe List(1, 2, 3)
    }

    ".dropWhile should return List(4, 5, 6) when f < " in {
      List.dropWhile(List(1, 2, 3, 4, 5, 6))(x => x < 4) shouldBe List(4, 5, 6)
    }

    ".dropWhile should return List(bee) when f is x.length == 1 " in {
      List.dropWhile(List("a", "bee", "ceee"))(s => s.length == 1) shouldBe List("bee", "ceee")
    }

//    ".dropWhile should return List() when f divisible by 2 " in {
//      List.dropWhile(List())(x => (x % 2) == 0) shouldBe List()
//    }

    // INIT

    ".init should return List()" in {
      List.init(List()) shouldBe List()
    }

    ".init should return List() when it has one element" in {
      List.init(List(1)) shouldBe List()
    }

    ".init should return List(1, 2, 3)" in {
      List.init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
    }

    // LENGTH

    ".length should return 3" in {
      List.length(List(1, 2, 3)) shouldBe 3
    }

    ".length should return 2" in {
      List.length(List("ant", "bear")) shouldBe 2
    }

    ".length should return 1" in {
      List.length(List(1)) shouldBe 1
    }

    ".length should return 0 for an empty list" in {
      List.length(List()) shouldBe 0
    }

    ".sum3 should return 6 for an empty list" in {
      List.sum3(List(1, 2, 3)) shouldBe 6
    }

    ".product3 should return 6 for an empty list" in {
      List.product3(List(2, 3)) shouldBe 6
    }

    ".length2 should return 2 for an empty list" in {
      List.length2(List(2, 3)) shouldBe 2
    }

    // reverse

    ".reverse should return List(3, 2, 1)" in {
      List.reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
    }

    ".append should return List(1, 2, 3, 1)" in {
      List.append2(List(1, 2, 3), List(1)) shouldBe List(1, 2 , 3, 1)
    }

    ".mapLeft should return List ( 2, 4, 6)" in {
      List.mapLeft(List(1, 2, 3))(x => x * 2) shouldBe List(2, 4, 6)
    }

    ".addOne2 should return List ( 2, 3, 4)" in {
      List.addOne2(List(1, 2, 3)) shouldBe List(2, 3, 4)
    }

    ".flatMap should return List ( 1, 1, 2, 2, 3, 3)" in {
      List.flatMap(List(1, 2, 3))(x => List(x, x)) shouldBe List(1, 1, 2, 2, 3, 3)
    }

    ".flatMapWithFilter should return List(2, 4)" in {
      List.flatMapWithFilter(List(1, 2, 3, 4))(x => x % 2 == 0) shouldBe List(2, 4)
    }

    ".add2List should return List ( 2, 4, 6)" in {
      List.add2List(List(1, 2, 3), List(1, 2, 3)) shouldBe List(2, 4, 6)
    }

    ".add2List should return List ( 5, 7, 9)" in {
      List.add2List(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
    }

    ".zipWith should return List( aA, bB, cC)" in {
      List.zipWith(List("a", "b", "c"), List("A", "B", "C"))(_ + _) shouldBe List("aA", "bB", "cC")
    }

    ".zipWith should return List( 14, 25, 36)" in {
      List.zipWith(List(1, 2, 3), List(4, 5, 6))(_.toString + _.toString) shouldBe List("14", "25", "36")
    }

    ".hasSubsequence should return true" in {
      List.hasSubsequence(List(1, 2, 3), List(1, 2)) shouldBe true
    }

    ".hasSubsequence should also return true" in {
      List.hasSubsequence(List(1, 2, 3), List(2, 3)) shouldBe true
    }

    ".hasSubsequence should return false" in {
      List.hasSubsequence(List(1, 2, 3), List(1, 3)) shouldBe false
    }

  }

}
