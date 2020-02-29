package chapter2

object IsSorted extends App {

  def sorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(n: Int): Boolean =
      if (n >= as.length -1) true
      else if (ordered(as(n), as(n + 1)) == false) false
      else go(n +1)
    go(0)
  }
}
