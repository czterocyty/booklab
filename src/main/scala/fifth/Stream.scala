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

  // 5.7
  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty: Stream[B])((e, z) => Stream.cons(f(e), z))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty: Stream[A])((e, z) => {
      if (p(e)) {
        Stream.cons(e, z)
      } else {
        z
      }
    })
  }

  def append[B >: A](s: Stream[B]): Stream[B] = {
    foldRight(s)((e, z) => Stream.cons(e, z))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty: Stream[B])((e, z) => f(e).append(z))
  }

  def find(p: A => Boolean): Option[A] = {
    filter(p).headOption
  }

  // 5.13
  def map_byUnfold[B](f: A => B): Stream[B] = {
    def g(s: Stream[A]): Option[(B, Stream[A])] = {
      s match {
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
      }
    }
    Stream.unfold(this)(g)
  }

  def take_byUnfold(n: Int): Stream[A] = {
    def g(s: (Stream[A], Int)): Option[(A, (Stream[A], Int))] = {
      s match {
        case (Cons(h, t), c) if c > 0 => Some(h(), (t(), c-1))
        case _ => None
      }
    }

    Stream.unfold((this, n))(g)
  }

  def takeWhile_byUnfold(p: A => Boolean): Stream[A] = {
    def g(s: Stream[A]): Option[(A, Stream[A])] = {
      s match {
        case Cons(h, t) if p(h()) => Some(h(), t())
        case _ => None
      }
    }

    Stream.unfold(this)(g)
  }

  def zipWith[B >: A, C](b: Stream[B])(f: (A, B) => C): Stream[C] = {
    def g(s: (Stream[A], Stream[B])): Option[(C, (Stream[A], Stream[B]))] = {
      s match {
        case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
        case _ => None
      }
    }

    Stream.unfold((this, b))(g)
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    def g(s: (Stream[A], Stream[B])): Option[((Option[A], Option[B]), (Stream[A], Stream[B]))] = {
      s match {
        case (Empty, Empty) => None
        case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), Empty))
        case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (Empty, tb()))
        case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
      }
    }

    Stream.unfold((this, s2))(g)
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

   // 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val f: Stream[A] = Stream.cons(a, f)
    f
  }

  // 5.9
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n+1))
  }

  // 5.10
  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      lazy val next = a + b
      Stream.cons(a, go(b, next))
    }
    go(0, 1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(o: Option[(A, S)]): Stream[A] = {
      if (o.isDefined) {
        lazy val next = o.get._2
        Stream.cons(o.get._1, go(f(next)))
      } else {
        Stream.empty
      }
    }
    go(f(z))
  }

  // 5.12
  def fibs_byUnfold(): Stream[Int] = {
    unfold((0, 1): (Int, Int))(t => Some((t._1, (t._2, t._1 + t._2))))
  }

  def from_byUnfold(n: Int): Stream[Int] = {
    unfold(n)(s => Some(s, s+1))
  }

  def constant_byUnfold[A](a: A): Stream[A] = {
    unfold(a)(s => Some(s, s))
  }
}

