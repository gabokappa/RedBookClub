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

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // 6.5

  def double4: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // book version below

  val _double: Rand[Double] =
    map(nonNegativeInt)(_ /(Int.MaxValue.toDouble+ 1))

  // exercise 6.6 a combinator to combine two RNG actions into one using a binary rather than unary function

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  // book examples

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  //below method not working needs to be reversed
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft(unit(List[A]()))((acc, f) => map2(f, acc)(_ :: _))
  }

  // book example

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1) // new state being passed along
    }

  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

def _map[A, B](s: Rand[A])(f: A => B) =
  flatMap(s)(a => unit(f(a)))

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))( _ + 1)


  //// my own println tests below

 val rng = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt
  println(n1)
  println(rng2)
  println(rng2.nextInt)

  println(nonNegativeInt(rng))
  println(double(rng))
  println(16159453 / (Int.MaxValue.toDouble + 1))
  println(intDouble(rng))
println(s" this is non negative even method ${nonNegativeEven(rng)}")
  println(int(rng))

  val x = int(rng)
  println(int(x._2))

}
