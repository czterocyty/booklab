package third

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) {
      Nil
    } else {
      Cons(as.head, apply(as.tail: _*))
    }
  }

  def tail[A](list: List[A]): List[A] = {
    list match {
      case Cons(_, t) => t
      case _ => list
    }
  }

  def setHead[A](list: List[A], head: A): List[A] = {
    list match {
      case Cons(_, t) => Cons(head, t)
      case _ => list
    }
  }

  @tailrec
  def drop[A](list: List[A], n: Int): List[A] = {
    if (n <= 0) {
      list
    } else {
      list match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
    }
  }

  def init[A](list: List[A]): List[A] = {
    def append(l: List[A], next: A): List[A] = {
      l match {
        case Nil => Cons(next, Nil)
        case Cons(h, t) => Cons(h, append(t, next))
      }
    }

    @tailrec
    def loop(l: List[A], acc: List[A]): List[A] = {
      l match {
        case Nil => acc
        case Cons(_, Nil) => acc
        case Cons(h, t) => loop(t, append(acc, h))
      }
    }

    loop(list, Nil)
  }
}
