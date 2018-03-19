package fifth

sealed trait Stream[+A] {
  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }
  }

  // 5.1
  def toList: List[A] = {
    this match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }
  }

  // 5.2
  def take(n: Int): Stream[A] = {
    if (n <= 0) {
      Empty
    } else {
      this match {
        case Empty => Empty
        case Cons(h, t) => Cons(h, () => t().take(n-1))
      }
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(_, t) if n > 0 => t().drop(n-1)
      case Cons(h, t) => Cons(h, t)
    }
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) => {
        if (p(h())) {
          Cons(h, () => t().takeWhile(p))
        } else {
          Empty
        }
      }
    }
  }

  def exists(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case Empty => true
      case _ => false
    }
  }

  // 5.5
  def takeWhile_byFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((e, z) => {
      if (!p(e)) {
        z
      } else {
        Cons(() => e, () => z)
      }
    })
  }

  // 5.6
  def headOption_byFoldRight(): Option[A] = {
    foldRight(None: Option[A])((e, _) => {
      Some(e)
    })
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) {
      empty
    } else {
      cons(as.head, apply(as.tail: _*))
    }
  }
}

