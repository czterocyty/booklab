package second

import org.scalatest.FunSuite

class IndianFlavourTest extends FunSuite {

  test("curry") {
    val concat: (Int, Int) => String = (a, b) => a + "+" + b

    val intToIntToString = IndianFlavour.curry(concat)

    assert(intToIntToString(2)(5) == "2+5")
  }

  test("uncurry") {
    val concat: Int => Int => String = (a) => (b) => a + "+" + b

    val f = IndianFlavour.uncurry(concat)

    assert(f(2, 5) == "2+5")
  }
}
