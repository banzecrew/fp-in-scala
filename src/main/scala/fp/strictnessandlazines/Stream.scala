package fp.strictnessandlazines

import scala.collection.immutable

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  //5.1[X]
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => Nil
  }

  //5.2[X]
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => Cons(() => h(), () => t().take(n - 1))
    case _ => Empty
  }
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => t().drop(n - 1)
    case _ => this
  }

  //5.3[X]
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => Cons(() => h(), () => t().takeWhile(p))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  //5.4[X]
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  //5.5[X]
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(this)((a, b) => if (p(a)) Stream.cons(a, b) else Empty)

  //5.6[X]
  def headOptionViaFoldRight: Option[A] = 
    foldRight(None: Option[A])((h, _) => Some(h))

  //5.7[X]
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def append[B>:A](xs: => Stream[B]): Stream[B] =
    foldRight(xs)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tl)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)
  
  //5.8[X]
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  //5.9[X]
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  //5.10[X]
  def fibs: Stream[Int] = {
    def f(fib1: Int, fib2: Int): Stream[Int] =
      cons(fib1, f(fib2, fib1 + fib2))
    f(0, 1)
  }

  //5.11[X]
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, t)) => cons(h, unfold(t)(f))
    case None => empty
  }

  //5.12[X]
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)){ case ((x, y)) => Some((x, (y, x + y))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(v => Some((v, v + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(v => Some((v, v)))

  def onesViaUnfold: Stream[Int] =
    unfold(1)(v => Some((v, v)))
}