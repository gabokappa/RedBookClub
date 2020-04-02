package chapter5

import chapter4.ErrorsAndExceptions._

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def toListLazy: List[A] = {
    val buffer = new collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def loop(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buffer += h()
        loop(t())
      case _ => buffer.toList
    }

    loop(this ())
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    // if n is greater than 1, you want to recursively loop through the tail, removing n elements until n is equal to 1
    // [1,2,3] becomes Cons(1, [2,3]), becomes [1] :: Cons(2, 3) ---- if n is 2, you end up with { 1 :: Cons(2, empty) }
    case Cons(h, _) if n == 1 => cons(h(), empty)
    // once n reaches 1, you want to return only the head (which can be composed of multiple Cons branches)
    case _ => empty
    // if n < 1, you won't return any Stream elements
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    // if  n is non-zero, you want to return the tail of the list n times =>
    // [1,2,3] tail becomes [2,3] tail becomes [3]
    case _ => this
    // Or else just return the same stream, since you're not dropping a positive integer of elements
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    // if the predicate is true for Head, keep the head and recursively check the head of each tail.
    // Halt recursion and set the tail to empty once the predicate is false
    // For [1,2,3] takeWhile( A < 3 ), we get 1 :: [2,3], then 1 :: 2 :: empty, which is equivalent to [1,2]
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = {

    @annotation.tailrec
    def loop(n: Int, s: Stream[A]): Boolean = this match {
      case Cons(h, t) =>
        if (p(h()) && (n == 0)) {
          true
          // if we reach n == 0 without having returned a false, the expression must be true
        } else if (p(h()) && n != 0) {
          loop(n - 1, t())
          // if n is still not 0, we must loop until it is (whilst p(h()) remains true)
        }
        else false
      // if p(h()) ever evaluates to false, we return false immediately
      case _ => false
      // if this is not of the type Cons(h,t), we return false by default
    }

    loop(this.toListLazy.length, this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def takeWhileFR(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) =>
      // into an empty Stream, conditionally fold if the head matches a predicate, otherwise stop and return Cons(StreamSoFar, empty)
      if (p(h)) cons(h, t)
      else empty)
  }

  def mapFR[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) =>
      cons(f(h), t()))
    // just apply the parameterised function to every head element as you fold
    // [1,2,3].mapFR(_ * 2) -----> 2 + (foldRight(Cons(2, Cons(3, Nil))) ------> 2 + (4 + (foldRight(Cons(3,Nil))) ------> (2 + (4 + (6 + (0))))
  }

  def filterFR(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      // keeps head if f(h) is true
      else t
      // discards the head if (f(h)) evaluates to false
    )
  }

  def appendFR[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMapFR[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h).appendFR(t))
  }

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
    // cons method already constructs H & T lazily, so think we can use here to infinitely add a tail of constant a to the stream
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
    // same as the above constant method, except we increment by 1
  }

  def fibs: Stream[Int] = {
    def loop(current: Int, next: Int): Stream[Int] = {
      cons(current, loop(next, current + next))
    }

    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty
    }
    // If f(z) is non-terminating, we add to the stream the result of unfold(t)(f). If it is then we terminate the stream with an empty.
  }

  def fibsUF: Stream[Int] =
    unfold((0, 1)) {
      case (current, next) => Some((current, (next, current + next)))
      // we attempt an unfold with initial state current & next = (0, 1) , and pass it a function that handles additions to the stream
      // don't need to include a termination step, because it's handled by unfold
    }

  def fromUF(n: Int): Stream[Int] =
    unfold(n) {
      case value => Some(value, value + 1)
      // handles the value otherwise terminates via unfold if nothing is returned by this method.
    }


  def constantUF[A](a: A): Stream[A] = {
    unfold(a) {
      case value => Some(value, value)
      // state is always the same value, so both S and A should be value if the result of unfold is non-empty
    }
  }

  def onesUF[A](a: A): Stream[A] = {
    unfold(a) {
      case value => Some(value, value)
      // same as constant, but could be non-generalized (use explicitly the value 1, rather than passing in a param)
      // like unfold(1)(_ => Some(1, 1))
    }
  }

  def mapUF[B](f: A => B): Stream[B] = {
    unfold(this) {
      // no value parameter for map, so unfolding this
      case Cons(h, t) => Some(f(h()), t())
      // apply the function continuously to the head of Stream until unfold terminates for None
    }
  }

  def takeUF(n: Int): Stream[A] = {
    unfold(this, n) {
      case (Cons(h, _), 1) => Some(h(), (empty, 0))
      // if n == 1, we just return the accumulated head
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
      // if n != 1, and we have a head and tail, we want to continue to increment by -1, creating a finite stream of heads and tails
      // not sure if we need a case _ => None?????
    }
  }

  def takeWhileUF(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      // if p is still true, we continue to take
      case Cons(h, _) if !p(h()) => Some(h(), empty)
      // if p isn't true, we can force termination with the accumulated h, and empty
    }
  }
}