package second

object IndianFlavour {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    // expanded form
    def a2f(a: A): B => C = {
      def b2c(b: B): C = {
        f(a, b)
      }
      b2c
    }
    a2f

    // (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
