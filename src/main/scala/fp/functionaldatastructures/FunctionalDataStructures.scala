package fp.functionaldatastructures

import scala.annotation.tailrec


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

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  //3.6[X]
  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case Nil => l
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    case Nil => z
  }

  def sum2(xs: List[Int]): Int =
    foldRight(xs, 0)(_ + _)

  def product2(xs: List[Double]): Double =
    foldRight(xs, 1.0)(_ * _)

  //3.7[X]
  /**
    * No, because foldRight traverse all elements
    * before they(elements) will be calculated
    */

  //3.8[X]
  /**
    * We get back our source list
    */

  //3.9[X]
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, ac) => ac + 1)

  //3.10[X]
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(xs: List[A], zz: B): B = xs match {
      case Cons(h, t) => loop(t, f(zz, h))
      case Nil => zz
    }
    loop(as, z)
  }

  //3.11[X]
  def foldLeftSum(xs: List[Int]): Int =
    foldLeft(xs, 0)(_ + _)
  def foldLeftProduct(xs: List[Double]): Double =
    foldLeft(xs, 1: Double)(_ * _)

  //3.12[X]
  def reverse[A](xs: List[A]): List[A] = {
    @tailrec
    def loop(xss: List[A], ac: List[A]): List[A] = xss match {
      case Cons(h, t) => loop(t, Cons(h, ac))
      case _ => ac
    }
    loop(xs, Nil)
  }
  def foldLeftReverse[A](xs: List[A]): List[A] =
    foldLeft(xs, List[A]())((ac, v) => Cons(v, ac))

  //3.13[ ]
  def foldRightViaFoldLeft[A, B](xs: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(xs), z)((ac, v) => f(v, ac))

  def foldRightViaFoldLeft2[A, B](xs: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(xs, (b: B) => b)((g, v) => b => g(f(v, b)))(z)

  def foldLeftViaFoldRight[A,B](xs: List[A], z: B)(f: (B,A) => B): B =
    foldRight(xs, (b:B) => b)((v, g) => b => g(f(b,v)))(z)

  //3.14[X]
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  //3.15[X]
  def concat[A](xs: List[List[A]]): List[A] = {
    @tailrec
    def loop(ll: List[List[A]], ac: List[A]): List[A] = ll match {
      case Cons(hh, tt) => loop(tt, append(ac, hh))
      case Nil          => ac
    }
    loop(xs, Nil)
  }
  def concat2[A](xs: List[List[A]]): List[A] =
    foldLeft(xs, List[A]())(append)

  //3.16[X]
  def inc(xs: List[Int]): List[Int] =
    foldRight(xs, Nil:List[Int])((v, ac) => Cons(v + 1, ac))

  //3.17[X]
  def asString(xs: List[Double]): List[String] =
    foldRight(xs, Nil:List[String])((v, ac) => Cons(s"$v", ac))

  //3.18[X]
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((v, ac) => Cons(f(v), ac))

  //3.19[X]
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((v, ac) => if (f(v)) Cons(v, ac) else ac)

  //3.20[X]
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  //3.21[X]
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  //3.22[X]
  def sum(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sum(t1, t2))
    case (_, Nil)                     => xs
    case (Nil, _)                     => ys
  }
  
  //3.23[X]
  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case _                            => Nil
  }

  //3.24[X]
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(h1, t1), Cons(h2, Nil)) => (h1 == h2)
    case (Cons(h1, t1), Cons(h2, t2)) =>
      if (h1 == h2) hasSubsequence(t1, t2)
      else hasSubsequence(t1, sub)
    case (_, Nil) => true
    case (Nil, Cons(h, t)) => false
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  //3.25[X]
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  //3.26[X]
  def maximum[A](t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  //3.27[X]
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  //3.28[X]
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  
  //3.29[X]
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def maximumViaFold[A](t: Tree[Int]): Int =
    fold(t)(x => x)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l, r) => (l max r) + 1)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)):Tree[B])(Branch(_, _))
  
}
