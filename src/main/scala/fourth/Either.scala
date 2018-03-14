package fourth

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B): Either[E, B] = {
    Left(value)
  }

  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = {
    Left(value)
  }

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = {
    b
  }

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = {
    this
  }
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Either[Nothing, B] = {
    Right(f(value))
  }

  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = {
    f(value)
  }

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this
  }

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    flatMap(ea => b.map(eb => f(ea, eb)))
  }
}

object Either {
  // 4.7
  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] = {
    val start: Either[E, List[A]] = Right(List())
    as.foldRight(start)((e, z) => e.flatMap(ev => z.map(l => ev :: l)))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    val start: Either[E, List[B]] = Right(List())
    as.foldRight(start)((e: A, z: Either[E, List[B]]) => f(e).flatMap(eo => z.map(l => eo :: l)))
  }
}


