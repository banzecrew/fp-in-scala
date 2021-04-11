package fp.gettingstarted

import scala.annotation.tailrec

object GettingsStarted {

  //2.1
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, next: Int): Int =
      if (n <= 1) prev
      else go(n - 1, next, prev + next)

    go(n, 0, 1)
  }
}