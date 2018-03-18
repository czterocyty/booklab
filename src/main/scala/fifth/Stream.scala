package fifth

import scala.annotation.tailrec

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
  def take(n: Int): List[A] = {
    if (n <= 0) {
      List.empty
    } else {
      this match {
        case Empty => List.empty
        case Cons(h, t) => h() :: t().take(n-1)
      }
    }
  }

  def drop(n: Int): List[A] = {
    if (n <= 0) {
      toList
    } else {
      this match {
        case Empty => List.empty
        case Cons(_, t) => t().drop(n-1)
      }
    }
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

