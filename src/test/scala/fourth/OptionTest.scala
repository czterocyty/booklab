package fourth

import org.scalatest.FunSpec

class OptionTest extends FunSpec {

  describe("construction") {
    it("Some is not equal to None") {
      val none: Option[Int] = None
      assert(Some(4) != none)
    }
  }

  describe("4.1 default methods") {
    it("filter") {
      assert(Some(5).filter(_ == 4) == None)
      assert(None.filter(_ == 3) == None)
    }

    it("orGet") {
      assert(Some(5).orElse(Some(4)) == Some(5))
      assert(None.orElse(Some(4)) == Some(4))
    }

    it("flatMap") {
      assert(Some(5).flatMap(a => Some(a * 2)) == Some(10))
      assert((None: Option[Int]).flatMap(a => Some(a * 2)) == None)
    }
  }

  it("4.2 variance") {
    assert(Option.variance(List(2.0, 3.0)) == Some(0.25))
  }

  it("4.3 map2") {
    assert(Option.map2(Some("A"), Some("B"))(_ + _) == Some("AB"))
    assert(Option.map2(Some("A"), None)(_ + _) == None)
    assert(Option.map2(None: Option[String], Some("B"))(_ + _) == None)
  }

  it("4.4 sequence") {
    assert(Option.sequence(List(Some("A"), Some("B"))) == Some(List("A", "B")))
    assert(Option.sequence(List(Some("A"), None)) == None)
    assert(Option.sequence(List()) == Some(List()))
  }

  it("4.5 traverse") {
    def f(a: String): Option[String] = {
      if (a.length == 1) {
        Some(a)
      } else {
        None
      }
    }

    assert(Option.traverse(List("A", "B"))(f) == Some(List("A", "B")))
    assert(Option.traverse(List("", "B"))(f) == None)
    assert(Option.traverse(List())(f) == Some(List()))
  }
}
