package fp.handlingerrorswithoutexceptions

import scala.{Either => _, _}


sealed trait Either[+E, +A] {
  //4.6[X]
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(e)  => Left(e)
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(e)  => Left(e)
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case Left(_)  => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
    Either[EE, C] = for {
      a <- this
      b <- b
    } yield f(a, b)

  def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
    Either[EE, C] = (this, b) match {
      case (Right(v1), Right(v2)) => Right(f(v1, v2))
      case (_, Left(e))           => Left(e)
      case (Left(e), _)           => Left(e)
    }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](x: => A): Either[Exception, A] =
    try Right(x)
    catch { case e: Exception => Left(e) }

  import fp.handlingerrorswithoutexceptions.Option._

  def parseInsuranceRateQuote(
      age: String,
      numberOfSpeedingTickets: String): Either[Exception,Double] =
    for {
      a <- Try { age.toInt }
      tickets <- Try { numberOfSpeedingTickets.toInt }
    } yield insuranceRateQuote(a, tickets)

  //4.7[X]
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
    traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
      case Nil => Right(Nil)
    }

  //4.8[X]
  /*
  We can accumulate multiple errors with:
    sealed trait Either[Seq[+E], A]

  or we can use:
    trait Either[+E ,+A]
    case class Value[+A](value: A) extends Partial[Nothing, A]
    case class Errors[+E](errors: Seq[E])] extends Partial[E, Nothing]

  

  */

}

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))


  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))


  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}