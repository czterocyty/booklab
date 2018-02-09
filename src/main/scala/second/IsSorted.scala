package second

import scala.annotation.tailrec

object IsSorted {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int, previous: Boolean): Boolean = {
      if (!previous) {
        false
      } else if (i == as.length) {
        previous
      } else if (i == 0) {
        loop(1, true)
      } else {
        val current = ordered(as(i-1), as(i))
        loop(i+1, previous & current)
      }
    }

    loop(0, true)
  }
}
