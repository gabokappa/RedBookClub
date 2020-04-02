package chapter4


object ErrorsAndExceptions {

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f)getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(x) => x
    }


    def orElse[B >: A](ob: => Option[B]): Option[B] =
      this.map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] =
      flatMap(a => if (f(a)) Some(a) else None)

    def filter2(f: A => Boolean): Option[A] = this match{
      case Some(a) if f(a) => this
      case _ => None
    }

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs) flatMap (m => mean(xs.map(x => math.pow(x -m, 2))))

    def lift[A, B] (f: A => B): Option[A] => Option[B] = _.map(f)

    val absO: Option[Double] => Option[Double] = lift(math.abs)

    def Try[A](a: => A): Option[A] =
      try Some(a)
    catch { case e: Exception => None }

    def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
      (numberOfSpeedingTickets / age) * 2.0

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)

    map2(optAge, optTickets)(insuranceRateQuote)
  }

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a flatMap(aa => b map(bb => f(aa, bb)))


    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap(hh => sequence(t) map(hh :: _))
    }


    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))( _ :: _)
    }

    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

    def altMap2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      for {
        aa <- a
        bb <- b
      } yield f(aa, bb)


  }

  sealed trait Either[+E, +A]
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](values: A) extends Either[Nothing, A]


  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

}
