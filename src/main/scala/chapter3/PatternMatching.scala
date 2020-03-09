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
      foldLeft(ls, 0)((x, _) => 1 + x)

    def reverse[A](ls: List[A]) = {
      foldLeft(ls, List[A]())((x, y) => Cons(y, x))
    }

    def flViaFR[A,B](l: List[A], z: B)(f: (B,A) => B): B =
      foldRight(l, (b: B) => b)((a,g) => b => g(f(b,a)))(z)

    def frViaFL[A,B](l: List[A], z: B)(f: (A,B) => B): B =
      foldLeft(reverse(l), z)((b,a) => f(a,b))

    def append2[A](l: List[A], r: List[A]): List[A] =
      foldRight(l, r)(Cons(_, _))

//    def append3[A](l: List[A], r: List[A]): List[A] =
//      foldLeft(l, r)(Cons(l, Cons(r, l)))

    def concatenate[A](l: List[List[A]]): List[A] =
      foldRight(l, Nil: List[A])(append)

    def addOne(ls: List[Int]) =
      foldRight(ls, Nil: List[Int])((x, xs) => Cons(x + 1, xs))

    def addOne2(ls: List[Int]) =
      foldLeft(reverse(ls), Nil: List[Int])((acc, x) => Cons(x +1, acc))

    def doubleToString(ls: List[Double]): List[String] =
      foldRight(ls, Nil: List[String])((x, xs) => Cons(x.toString, xs))

    def map[A, B](ls: List[A])(f: A => B): List[B] =
      foldRight(ls, Nil: List[B])((x, xs) => Cons(f(x), xs))

    def mapLeft[A, B](ls: List[A])(f: A => B): List[B] =
      foldLeft(reverse(ls), Nil: List[B])((acc, x) => (Cons(f(x), acc)))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)

    def filterLeft[A](as: List[A])(f: A => Boolean): List[A] =
      foldLeft(as, Nil: List[A])((xs, x) => if (f(x)) Cons(x, xs) else xs)

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      concatenate(map(as)(f))

    def flatMapWithFilter[A](ls: List[A])(f: A => Boolean): List[A] =
      flatMap(ls)(x => if (f(x)) List(x) else Nil)

    def add2List(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (Nil, _) => Nil
      case(_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, add2List(t1, t2))
    }

    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
      case (_, Nil) => true
      case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
      case _ => false
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }


  }

  println(List.sum(List(1, 2, 3)))
  println(List.product(List(2.0, 3.0, 5.0)))


//  val chap38 = List.foldLeft(List(1, 2, 3), Nil: List[Int])((acc: Int , y: List[Int])=> Cons(y , acc))
//  println(chap38)


}
