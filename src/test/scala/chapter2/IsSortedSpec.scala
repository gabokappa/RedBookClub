package chapter2

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class IsSortedSpec extends AnyFreeSpec with Matchers {

  private val intSorted = (x: Int, y: Int) => x < y
  private val stringSorted = (a: String, b: String) => a.length < b.length

  "IsSorted" - {
    "should return true if Array(0, 1)" in {
      IsSorted.sorted(Array(0, 1), intSorted) shouldBe true
    }

    "should return true if Array(0, 1, 4)" in {
      IsSorted.sorted(Array(0, 1, 4), intSorted) shouldBe true
    }

    "should return false if Array(0, 5, 2)" in {
      IsSorted.sorted(Array(0, 5, 2), intSorted) shouldBe false
    }

    "should return true with anonymous function if Array(1, 0)" in {
      IsSorted.sorted(Array(1, 0),(x: Int, y:Int) => x > y ) shouldBe true
    }

    "should return true if Array(a, bee)" in {
      IsSorted.sorted(Array("a", "bee"), stringSorted) shouldBe true
    }

    "should return true if Array(bee, a)" in {
      IsSorted.sorted(Array("bee", "a"), (x: String, y: String) => x.length > y.length) shouldBe true
    }

    "should return false if Array(bee, a)" in {
      IsSorted.sorted(Array("bee", "a"), stringSorted) shouldBe false
    }

    "should return true if Array(a, bee, ceee)" in {
      IsSorted.sorted(Array("a", "bee", "ceee"), stringSorted) shouldBe true
    }

  }
}
