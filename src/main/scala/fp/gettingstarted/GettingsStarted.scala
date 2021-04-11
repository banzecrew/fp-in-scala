package fp.gettingstarted

import scala.annotation.tailrec

object GettingsStarted {

  //2.1[ ]
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, next: Int): Int =
      if (n <= 1) prev
      else go(n - 1, next, prev + next)

    go(n, 0, 1)
  }

  //2.2[X]
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false

    loop(0)
  }

  //2.3[X]
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  //2.4[ ]
  def uncurry[A,B,C](f: A => (B => C)): (A, B) => C =
    (a, b) => f(a)(b)
}