package second

import scala.annotation.tailrec

object Fibbonaci {

  def fib(n: Int): Int = {
    @tailrec
    def sum(previous: Int, current: Int, n: Int): Int = {
      if (n == 0) {
        current
      } else {
        sum(current, previous + current, n - 1)
      }
    }

    if (n == 0) {
      0
    } else if (n == 1) {
      1
    } else {
      sum(0, 1, n - 1)
    }
  }

}
