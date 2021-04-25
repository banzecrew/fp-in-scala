package fp.handlingerrorswithoutexceptions

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
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]