package chapter3

import chapter3.Tree.{Branch, Leaf, TheTree}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TheTreeSpec extends AnyFreeSpec with Matchers {

  "Tree " - {
    ".size should count the leaves and branches in a tree aka number of nodes" in {
      val t = Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))
      TheTree.size(t) shouldBe 5
    }

    ".size should count all the leaves and branches in a tree aka number of nodes" in {
      val t = Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Leaf(1)))
      TheTree.size(t) shouldBe 7
    }

    ".max should take the maximum value within the tree" in {
      val t = Branch(Branch(Leaf(10), Leaf(5)), Leaf(9))
      TheTree.maximum(t) shouldBe 10
    }

    ".depth should return path length from tree to leaf" in {
      val t = Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))), Branch(Leaf(1), Leaf(1)))
      TheTree.depth(t) shouldBe 3
    }

    ".map modifies each element in a leaf with a given function" in {
      val t = Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))), Branch(Leaf(1), Leaf(1)))
      TheTree.map(t)(x => x * 3) shouldBe Branch(Branch(Leaf(3), Branch(Leaf(3), Leaf(3))), Branch(Leaf(3), Leaf(3)))
    }

    ".sizeFold counts leaves and branches aka the node via fold" in {
      val t = Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))), Branch(Leaf(1), Leaf(1)))
      TheTree.sizeFold(t) shouldBe 9
    }

    ".maximumFold returns the maximum value in the tree ie the Leaf" in {
      val t = Branch(Branch(Leaf(10), Leaf(5)), Leaf(9))
      TheTree.maximumFold(t) shouldBe 10
    }

    ".depthFold returns path length from tree to leaf" in {
      val t = Branch(Branch(Leaf(10), Leaf(5)), Leaf(9))
      TheTree.depthFold(t) shouldBe 2
    }

    ".mapFold modifies each element in a leaf" in {
      val t = Branch(Branch(Leaf(10), Leaf(5)), Leaf(9))
      TheTree.mapFold(t)(x => x * 2) shouldBe Branch(Branch(Leaf(20), Leaf(10)), Leaf(18))
    }

    ".mapFold modifies each element in a leaf to a boolean" in {
      val t = Branch(Branch(Leaf(10), Leaf(5)), Leaf(9))
      TheTree.mapFold(t)(_ % 2 == 0) shouldBe Branch(Branch(Leaf(true), Leaf(false)), Leaf(false))
    }



  }

}
