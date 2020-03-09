package chapter3

object Tree extends App {

  sealed trait TheTree[+A]
  case class Leaf[A](value: A) extends TheTree[A]
  case class Branch[A] (left: TheTree[A], right: TheTree[A]) extends TheTree[A]

  object TheTree {

    def size[A](tr: TheTree[A]): Int = tr match {
      case Leaf(_) => 1
      case Branch(lef, rig) => 1 + size(lef) + size(rig)
    }

    def maximum(tr: TheTree[Int]): Int =
      tr match {
        case Leaf(n) => n
        case Branch(lef, rig) => maximum(lef) max maximum(rig)
      }

    def depth[A](tr: TheTree[A]): Int =
      tr match {
        case Leaf(_) => 0
        case Branch(lef, rig) => 1 + (depth(lef) max depth(rig))
      }

    def map[A, B](tr: TheTree[A])(f: A => B): TheTree[B] =
      tr match {
        case Leaf(x) => Leaf(f(x))
        case Branch(lef, rig) => Branch(map(lef)(f), map(rig)(f))
      }

    def fold[A,B](t: TheTree[A])(f: A => B)(g: (B, B) => B): B =
      t match {
        case Leaf(a) => f(a)
        case Branch( lef, rig) => g(fold(lef)(f)(g), fold(rig)(f)(g))
      }

    def sizeFold[A](t: TheTree[A]): Int =
      fold(t)(a => 1)(1 + _ + _)

    def maximumFold(t: TheTree[Int]): Int =
      fold(t)(n => n)(_ max _)

    def depthFold[A](tr: TheTree[A]): Int =
      fold(tr)(a => 0)((d1, d2) => 1 + (d1 max d2))

    def mapFold[A, B](t: TheTree[A])(f: A => B): TheTree[B] =
      fold(t)(a => Leaf(f(a)): TheTree[B])(Branch(_, _))

  }
}
