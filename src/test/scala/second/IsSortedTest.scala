package second

import org.scalatest.FunSuite

class IsSortedTest extends FunSuite {

  def isOrdered(a: Int, b: Int): Boolean = {
    a <= b
  }

  test("empty is always sorted") {
    assert(IsSorted.isSorted(Array(), isOrdered))
  }

  test("one elemental is always sorted") {
    assert(IsSorted.isSorted(Array(4), isOrdered))
  }

  test("two elemental is not sorted") {
    assert(!IsSorted.isSorted(Array(8, 4), isOrdered))
  }

  test("two elements are sorted") {
    assert(IsSorted.isSorted(Array(4, 8), isOrdered))
  }

  test("four elements") {
    assert(IsSorted.isSorted(Array(0, 0, 4, 4), isOrdered))
    assert(!IsSorted.isSorted(Array(0, 4, 4, 0), isOrdered))
  }
}
