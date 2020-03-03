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
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

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

    def setHead[A](lst: List[A], h: A): List[A] = {
      lst match {
        case Nil => Cons(h, Nil)
        case Cons(_, y) => Cons(h, y)
      }
    }

    def drop[A](l: List[A], n: Int): List[A] =
      if (n <= 0) l
      else {
        l match {
          case Nil => Nil
          case Cons(_, t) => drop(t, n -1)
        }
      }

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
      l match {
        case Nil => Nil
        case Cons(h, t) if f(h) => dropWhile(t)(f)
        case _ => l
      }

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t, a2))
      }

    def init[A](l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum2(ns: List[Int]) =
      foldRight(ns, 0)((x, y) => x + y)
    // could also be foldRight(ns, 0)(_ + _)

    def product2(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _)

    def length[A](as: List[A]): Int =
      foldRight(as, 0)((_, y) => 1 + y)

    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }

    def sum3(lst: List[Int]) =
      foldLeft(lst, 0)(_ + _)

    def product3(ls: List[Double]) =
      foldLeft(ls, 1.0)(_ * _)

    def length2[A](ls: List[A]): Int =
      foldLeft(ls, 0)((x, y) => 1 + x)


  }

  println(List.sum(List(1, 2, 3)))
  println(List.product(List(2.0, 3.0, 5.0)))

  val chap38 = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
  println(chap38)


}
