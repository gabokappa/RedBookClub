package chapter5

import chapter5.Stream._

sealed trait Stream[+A] {

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case  Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _=> this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h,t) else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Option(h))

  // had to change this from Some(h)

  def map[B](f: A => B): Stream [B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

//  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n +1 ))

  def fibs: Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] =
      cons(n1, go(n2, n1 + n2))
    go(0, 1)
  }

def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
  f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None: Option[String] => empty

      // the above case _ didn't accept None
  }
}

  val fibsUnfolded = {
    unfold((0, 1)) { case (n1, n2) => Option((n1, (n2, n1 + n2)))}
  }

  def fromUnfolded(n: Int): Stream[Int] =
    unfold(n)(n => Option((n, n+ 1)))

  def constantUnfolded[A](a: A) =
    unfold(a)(_ => Option((a, a)))


  def mapUnfolded[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeUnfolded(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileUnfolded(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if f(h()) => Some((h(), t()))
    }

  def zipWithUnfolded[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
  zipWithUnfolded(s).takeWhileUnfolded(!_._2.isEmpty) forAll {
    case (h,h2) => h == h2
  }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubSequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

def scanRight[B](z:B)(f: (A => B) => B): Stream[B] =
  foldRight((z, Stream(z)))((a, p0) => {
    lazy val p1 = p0
    val b2 = f(a, p1._1)
    (b2, cons(b2, p1._2))
  })._2



}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}

object main extends App {

 val a =  Stream(1, 2, 3, 4, 5)

  println(a.toList)
  println(s"this is a ${a}")
  println(Stream(1,2,3).take(2))
  println(Stream(1,2,3).take(2).toList)
  println(s"This is drop ${Stream(1,2,3,4,5).drop(2).toList}")
  println(s"This is takeWhile ${Stream(1,2,3,4,5).takeWhile(_ < 3).toList}")

val infi: Stream[Int] = Stream.cons(1, infi)
  println(infi.map(_ + 1).exists(_ % 2 == 0))

  println(infi.fibs.take(9).toList)

}