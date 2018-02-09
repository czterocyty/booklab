package second

import org.scalatest.FunSuite

class FibbonaciTest extends FunSuite {

  test("Zero") {
    assert(Fibbonaci.fib(0) == 0)
  }

  test("One") {
    assert(Fibbonaci.fib(1) == 1)
  }

  test("Two") {
    assert(Fibbonaci.fib(2) == 1)
  }

  test("Three") {
    assert(Fibbonaci.fib(3) == 2)
  }

  test("Four") {
    assert(Fibbonaci.fib(4) == 3)
  }

  test("Five") {
    assert(Fibbonaci.fib(5) == 5)
  }

  test("Six") {
    assert(Fibbonaci.fib(6) == 8)
  }

}
