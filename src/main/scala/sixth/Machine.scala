package sixth

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def coin(): Machine = {
    if (candies == 0 || !locked) {
      this
    } else {
      Machine(false, candies, coins)
    }
  }

  def knob(): Machine = {
    if (candies == 0 || locked) {
      this
    } else {
      Machine(true, candies - 1, coins + 1)
    }
  }

  def toTuple = (candies, coins)
}

object Machine {

  type MT = State[Machine, (Int, Int)]

  // 6.11
  def simulate(inputs: List[Input]): MT = {
    val initial: MT = State(m => (m.toTuple, m))
    def c(state: MT, input: Input): MT = {
      input match {
        case Coin => state
          .flatMap((_) => State(m => {
            val n = m.coin()
            (n.toTuple, n)
          }))
        case Turn => state
          .flatMap((_) => State(m => {
            val n = m.knob()
            (n.toTuple, n)
          }))
      }
    }

    inputs.foldLeft(initial)(c)
  }
}

