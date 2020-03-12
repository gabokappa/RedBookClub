//package chapter4
//
//object ErrorsAndExceptions extends App {
//
//  sealed trait Option[+A] {
//
//    def map[B](f: A => B): Option[B] = this match {
//      case None => None
//      case Some(x) => Some(f(x))
//    }
//
//    def flatMap[B](f: A => Option[B]): Option[B] =
//      map(f)getOrElse(None)
//
//    def getOrElse[B >: A](default: => B): B = this match {
//      case None => default
//      case Some(x) => x
//    }
//
//
//    def orElse[B >: A](ob: => Option[B]): Option[B] =
//      this.map(Some(_)).getOrElse(ob)
//
//    def filter(f: A => Boolean): Option[A] =
//      flatMap(a => if (f(a)) Some(a) else None)
//
//    def filter2(f: A => Boolean): Option[A] = this match{
//      case Some(a) if f(a) => this
//      case _ => None
//    }
//
//    def mean(xs: Seq[Double]): Option[Double] =
//      if (xs.isEmpty) None
//      else Some(xs.sum / xs.length)
//
//    def variance(xs: Seq[Double]): Option[Double] =
//      mean(xs) flatMap (m => mean(xs.map(x => math.pow(x -m, 2))))
//
//    def lift[A, B] (f: A => B): Option[A] => Option[B] = _.map(f)
//
//    val absO: Option[Double] => Option[Double] = lift(math.abs)
//
//    def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double]
//
//
//
//
//
//  }
//
//  case class Some[+A](get: A) extends Option[A]
//  case object None extends Option[Nothing]
//
//
//}
