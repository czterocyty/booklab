package third

import org.scalatest.FunSpec

class ListTest extends FunSpec {

  def matchit(list: List[Int]): Int = {
    list match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
  }

  describe("matchit") {
    it("output of match - default") {
      assert(matchit(List(1, 2, 3, 4, 5)) == 3)
    }

    it("output of match - Nil") {
      assert(matchit(Nil) == 42)
    }

    it("output of match - head") {
      assert(matchit(List(1, 2, 4)) == 1)
    }

    it("output of match - sum folded") {
      assert(matchit(List(1, 2, 3)) == 6)
      assert(matchit(List(1)) == 1)
    }

    it("output of match - last case") {
      // seems to be dead code
    }
  }

  describe("tail") {
    it("tail - empty") {
      assert(List.tail(Nil) == Nil)
    }

    it("tail - one element") {
      assert(List.tail(List("A")) == Nil)
    }

    it("tail - two elements") {
      assert(List.tail(List("A", "B")) == List("B"))
    }
  }

  describe("setHead") {
    it("setHead for empty") {
      assert(List.setHead(Nil, "a") == Nil)
    }

    it("setHead for one element") {
      assert(List.setHead(List("A"), "B") == List("B"))
    }

    it("setHead for more elements") {
      assert(List.setHead(List("A", "B"), "C") == List("C", "B"))
    }
  }

  describe("drop") {
    it("drop - empty") {
      assert(List.drop(Nil, 3) == Nil)
    }

    it("drop one-element") {
      assert(List.drop(List("A"), 1) == Nil)
    }

    it("drop nothing when 0") {
      assert(List.drop(List("A"), 0) == List("A"))
    }

    it("drop twice") {
      assert(List.drop(List(1, 2, 3), 2) == List(3))
    }
  }

  describe("init") {
    it("empty") {
      assert(List.init(Nil) == Nil)
    }

    it("one-element list") {
      assert(List.init(List(1)) == Nil)
    }

    it("two-elements list") {
      assert(List.init(List("A", "B")) == List("A"))
    }

    it("three-element list") {
      assert(List.init(List(5, 4, 1)) == List(5, 4))
    }
  }

  describe("product3") {
    it("1 * 1") {
      assert(List.product3(List(1.0, 1.0)) == 1.0)
    }

    it("0 * 1") {
      assert(List.product3(List(0.0, 1.0)) == 0.0)
    }
  }

  describe("foldRight and the constructor") {
    it("some elements") {
      val original = List("A", "B", "C")
      val list = List.foldRight(original, Nil: List[String])(Cons(_, _))

      assert(original == list)
    }
  }

  describe("length by foldRight") {
    it("empty list") {
      assert(List.length(Nil) == 0)
    }

    it("one element list") {
      assert(List.length(List("A")) == 1)
    }

    it("Some more-element array") {
      assert(List.length(List("A", "B", "C")) == 3)
    }
  }

  describe("some foldLeft") {

    def sum(a: String, b: Int): String = a + b

    it("sum using fold left") {
      assert(List.foldLeft(List(10, 9, 3), "")(sum) == "1093")
    }

    it("sum for empty list") {
      assert(List.foldLeft(Nil, "")(sum) == "")
    }

    it("sum for one element list") {
      assert(List.foldLeft(List(4), "")(sum) == "4")
    }
  }

  describe("3.11, some more foldLeft") {
    describe("productLeft") {
      it("empty") {
        assert(List.productLeft(Nil) == 1.0)
      }
    }

    describe("lengthLeft") {
      it("empty") {
        assert(List.lengthLeft(Nil) == 0)
      }
    }
  }

  describe("3.12 reverse") {
    it("empty list") {
      assert(List.reverse(Nil) == Nil)
    }

    it("one element list") {
      assert(List.reverse(List("A")) == List("A"))
    }

    it("more elements") {
      assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
    }
  }

  describe("3.13 foldLeft in terms of foldRight") {
    def sum(a: String, b: Int): String = a + b

    it("Empty list") {
      assert(List.foldLeftByFoldRight(Nil:List[Int], "")(sum) == "")
    }

    it("One element list") {
      assert(List.foldLeftByFoldRight(List(4), "")(sum) == "4")
    }

//    it("Three elements") {
//      assert(List.foldLeftByFoldRight(List(4, 5, 6), "")(sum) == "456")
//    }
  }
  
  describe("3.13 foldRight in terms of foldLeft") {
    def sum(a: Int, b: String): String = a + b

    it("Empty list") {
      assert(List.foldRightByFoldLeft(Nil:List[Int], "")(sum) == "")
    }

    it("One element list") {
      assert(List.foldRightByFoldLeft(List(4), "")(sum) == "4")
    }

//    it("Three elements") {
//      assert(List.foldRightByFoldLeft(List(4, 5, 6), "")(sum) == "456")
//    }
  }
}
