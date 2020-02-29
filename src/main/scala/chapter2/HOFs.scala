package chapter2

object HOFs extends App {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n * acc)
    go(n, 1)
  }

  def formatResult[A, B](name: A, n: B, f: B => B): String ={
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  println(formatResult("factorial", 5, factorial))

def findFirst[A](as: Array[A], f: A => Boolean): Int = {
  def loop(n: Int): Int =
    if (n >= as.length) -1
    else if (f(as(n))) n
    else loop(n + 1)
  loop(0)
}

}
