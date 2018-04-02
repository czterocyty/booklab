package sixth

// 6.10
case class State[S, +A](run: S => (A, S)) {

  def unit[B >: A](b: B): State[S, B] = State(s => (b, s))

  def map[B](f: A => B): State[S, B] = {
    State(s => {
      val (a, next) = run(s)
      (f(a), next)
    })
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(s => {
      val (a, next) = run(s)
      val (b, next2) = sb.run(next)
      (f(a, b), next2)
    })
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, next) = run(s)
      val (v, next2): (B, S) = f(a)(next)
      (v, next2)
    })
  }
}

object State {

  type State[S, +A] = S => (A, S)
}
