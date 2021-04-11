package fp.functionaldatastructures


sealed trait List[+A]
case class Cons[+A](x: A, xs: List[A]) extends List[A]
case object Nil extends List[Nothing]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](xs: A*): List[A] =
    if (xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))

  //3.1[X]
  //Result is 3
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // <- will do
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  //3.2[X]
  def tail[A](xs: List[A]): List[A] = xs match {
    case Cons(_, xs) => xs
    case _ => Nil
  }

  //3.3[X]
  def setHead[A](h: A, xs: List[A]): List[A] = xs match {
    case Cons(_, xs) => Cons(h, xs)
    case _           => Cons(h, Nil)
  }

  //3.4[X]
  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 1) l
    else l match {
      case Cons(_, xs) => drop(xs, n - 1)
      case _           => Nil
    }

  //3.5[X]
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
    case _                     => l
  }
}