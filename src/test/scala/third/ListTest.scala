package third

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  def matchTest(list: List[Int]): Int = {
    list match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
  }

  test("output of match - default") {
    assert(matchTest(List(1, 2, 3, 4, 5)) == 3)
  }

  test("output of match - Nil") {
    assert(matchTest(Nil) == 42)
  }

  test("output of match - head") {
    assert(matchTest(List(1, 2, 4)) == 1)
  }

  test("output of match - sum folded") {
    assert(matchTest(List(1, 2, 3)) == 6)
    assert(matchTest(List(1)) == 1)
  }

  test("output of match - last case") {
    // seems to be dead code
  }
}