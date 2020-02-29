package chapter2

object Fibonacci extends App {

  def fib(b: Int): Int = {
    @scala.annotation.tailrec
    def loop(b: Int, current: Int, prev: Int): Int = {
      if (b == 1) prev
      else loop(b - 1, current + prev, current)
    }
    loop(b, 1, 0)
  }
  println(s"${fib(13)}")
}
