package fp.handlingerrorswithoutexceptions

import scala.{Option => _}


sealed trait Option[+A] {
  //4.1[X]
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case _       => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case _       => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if (f(v)) => this
    case _                 => None
  }

  def filter2(f: A => Boolean): Option[A] =
    flatMap(x => if (f(x)) Some(x) else None)
}
object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  //4.2[X]
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
  
  val abs0: Option[Double] => Option[Double] = lift(math.abs)

  /**
  * Top secret formula for computing an annual car
  * insurance premium from two key factors.
  */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String): Option[Double] = {
      val optAge: Option[Int] = Try(age.toInt)
      val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
      map2(optAge, optTickets)(insuranceRateQuote)
    }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  //4.3[X]
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(v1), Some(v2)) => Some(f(v1, v2))
    case _                    => None
  }

  //4.4[X]
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    case Nil    => Some(Nil)
  }

  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt)))

  //4.5[X]
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence (a map f)

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case h :: t => map2(f(h), traverse2(t)(f))(_ :: _)
    case Nil => Some(Nil)
  }

  def map2_2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C):
    Option[C] =
      for {
        aa <- a
        bb <- b
      } yield f(aa, bb)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]