package third

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.25
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  // 3.26
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l).max(maximum(r))
    }
  }

  // 3.27
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => math.max(depth(r), depth(l)) + 1
    }
  }

  // 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  // 3.29
  def fold[A, B](tree: Tree[A])(f: A => B)(m: (B, B) => B): B = {
    tree match {
      case Branch(l, r) => m(fold(l)(f)(m), fold(r)(f)(m))
      case Leaf(v) => f(v)
    }
  }

  def mapByFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    def leafFn(v: A): Tree[B] = Leaf(f(v))
    def mergeFn(l: Tree[B], r: Tree[B]): Tree[B] = Branch(l, r)

    fold(tree)(leafFn)(mergeFn)
  }
}