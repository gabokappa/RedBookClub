package chapter6

trait RNG {
  def nextInt: (Int, RNG)

}

object RNG extends App {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n1, rng1) = rng.nextInt
    (if (n1 < 0) -(n1 + 1) else n1, rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nInt, rng1) = nonNegativeInt(rng)
    (nInt / (Int.MaxValue.toDouble + 1), rng1)
  }
  // Originally had double without the .toDouble on the Int.MaxValue so it was always returning a 0.0

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (x, s) = (nonNegativeInt(rng), double(rng))
    ((x._1, s._1), x._2)
  }

  // actual answer is intDouble2

  def intDouble2(rng: RNG): ((Int, Double), RNG) = {
    val (n1, r1) = rng.nextInt
    val (d1, r2) = double(r1)
    ((n1, d1), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i1, d1), r1) = intDouble2(rng)
    ((d1, i1), r1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(n: Int, l: List[Int], r1: RNG): (List[Int], RNG) =
      if (n == 0)
        (l, r1)
      else {
        val (i1, r2) = r1.nextInt
        loop(n -1, i1 :: l, r2)
      }
    loop(count, List(), rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def double4: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // book version below

  val _double: Rand[Double] =
    map(nonNegativeInt)(_ /(Int.MaxValue.toDouble+ 1))

 val rng = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt
  println(n1)
  println(rng2)
  println(rng2.nextInt)

  println(nonNegativeInt(rng))
  println(double(rng))
  println(16159453 / (Int.MaxValue.toDouble + 1))
  println(intDouble(rng))


}
