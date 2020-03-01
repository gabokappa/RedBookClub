package chapter3

object PatternMatching extends App {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }
// apply is a Variadic function
    def apply[A](as: A*): List[A] =
      if(as.isEmpty) Nil
      else Cons(as.head, apply(as.tail:_*))

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    def tail[A](lst: List[A]): List[A] =
      lst match {
        case Nil => sys.error("no tail on Nil")
        case Cons(_, t) => t
      }

  }

  println(List.sum(List(1, 2, 3)))
  println(List.product(List(2.0, 3.0, 5.0)))


}
