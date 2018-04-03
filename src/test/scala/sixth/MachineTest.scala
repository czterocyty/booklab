package sixth

import org.scalatest.FunSpec

class MachineTest extends FunSpec {

  it("no candies") {
    val inputs = List(Coin, Turn)
    val machine = Machine(false, 0, 10)

    val (result, _) = Machine.simulate(inputs).run(machine)

    assert(result == (0, 10))
  }

  it("happy path from book") {
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

    val machine = Machine(false, 5, 10)

    val (result, _) = Machine.simulate(inputs).run(machine)

    assert(result == (1, 14))
  }

  it("insert single coin to unlocked machine does nothing") {
    val inputs = List(Coin)

    val machine = Machine(false, 1, 1)

    val (result, s) = Machine.simulate(inputs).run(machine)

    assert(result == (1, 1))
    assert(!s.locked)
  }

  it("turn knob of locked machine does nothing") {
    val inputs = List(Turn)
    val machine = Machine(true, 1, 1)
    val (result, s) = Machine.simulate(inputs).run(machine)

    assert(result == (1, 1))
    assert(s.locked)
  }

  it("insert single coin to locked machine") {
    val inputs = List(Coin)
    val machine = Machine(true, 1, 1)

    val (result, s) = Machine.simulate(inputs).run(machine)

    assert(result == (1, 1))
    assert(!s.locked)
  }

  it("turn knob of unlocked machine dispenses candy") {
    val inputs = List(Turn)
    val machine = Machine(false, 1, 2)

    val (result, s) = Machine.simulate(inputs).run(machine)

    assert(result == (0, 3))
    assert(s.locked)
  }
}
