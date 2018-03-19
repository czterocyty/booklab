package fifth

import org.scalatest.{BeforeAndAfter, FunSpec}

class StreamTest extends FunSpec with BeforeAndAfter {

  var ntimes = 0

  after {
    ntimes = 0
  }

  describe("toList") {
    it("3-element stream") {
      assert(Stream('A', 'B', 'C').toList == List('A', 'B', 'C'))
    }
  }

  describe("take and drop") {
    describe("take") {
      it("3-element stream") {
        assert(Stream("A", "B", "C").take(2).toList == List("A", "B"))
      }

      it("n greather than stream size") {
        assert(Stream("A").take(2).toList == List("A"))
      }

      it("Empty") {
        assert(Stream.empty.take(1).toList == List.empty)
      }
    }
    
    describe("drop") {
      it("3-element stream") {
        assert(Stream("A", "B", "C").drop(2).toList == List("C"))
      }

      it("n greather than stream size") {
        assert(Stream("A").drop(2).toList == List())
      }

      it("Empty") {
        assert(Stream.empty.drop(1).toList == List.empty)
      }
    }
  }

  describe("takeWhile") {
    def p(s: String): Boolean = s < "C"

    it("3-element stream") {
      assert(Stream("A", "B", "C").takeWhile(p).toList == List("A", "B"))
    }

    it("n greather than stream size") {
      assert(Stream("A").takeWhile(p).toList == List("A"))
    }

    it("Empty") {
      assert(Stream.empty.takeWhile(p).toList == List.empty)
    }
  }

  describe("forAll") {
    it("success") {
      assert(Stream("A", "A", "A").forAll((s) => s.equals("A")))
    }

    it("non-success") {
      assert(!Stream("A", "B", "C").forAll(_ == "A"))
    }
  }

  describe("headOption by foldRight") {
    it("empty stream") {
      assert(Stream.empty.headOption_byFoldRight().isEmpty)
    }

    it("two elements") {
      assert(Stream("A", "B").headOption_byFoldRight() == Some("A"))
    }
  }

  describe("5.7, map") {
    def f(s: String): Int = {
      ntimes = ntimes+1
      s.length
    }

    it("empty stream") {
      assert(Stream.empty.map(f) == Stream.empty)
      assert(ntimes == 0)
    }

    it("some stream") {
      assert(Stream("A", "BB", "CCC").map(f).toList == List(1, 2, 3))
      assert(ntimes == 3)
    }
  }

  describe("5.7, filter") {

    def f(s: String): Boolean = {
      ntimes = ntimes+1
      s.length == 2
    }

    it("empty stream") {
      assert(Stream.empty.filter(f).toList == List())
      assert(ntimes == 0)
    }

    it("some stream") {
      assert(Stream("A", "BB", "CCC").filter(f).toList == List("BB"))
      assert(ntimes == 3)
    }
  }

  describe("5.7, append") {
    val s1 = Stream("A", "B", "C")
    val s2 = Stream("1", "2", "4")

    it("two empty streams") {
      assert(Stream.empty.append(Stream.empty) == Stream.empty)
    }

    it("empty with non-empty") {
      assert(Stream.empty.append(s1).toList == List("A", "B", "C"))
      assert(s1.append(Stream.empty).toList == List("A", "B", "C"))
    }

    it("non empty") {
      assert(s1.append(s2).toList == List("A", "B", "C", "1", "2", "4"))
    }
  }

  describe("5.7, flatMap") {
    def f(s: String): Stream[Int] = {
      Stream(s.length, s.codePointAt(0))
    }

    it("empty") {
      assert(Stream.empty.flatMap(f) == Stream.empty)
    }

    it("nonEmpty") {
      assert(Stream("A", "BB").flatMap(f).toList == List(1, 65, 2, 66))
    }
  }

  it("5.8 constant") {
    assert(Stream.constant("A").take(5).toList == List("A", "A", "A", "A", "A"))
  }

  it("5.9 from") {
    assert(Stream.from(3).take(5).toList == List(3, 4, 5, 6, 7))
  }

  it("5.10 fibs") {
    assert(Stream.fibs().take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  it("5.12 fibs by unfold") {
    assert(Stream.fibs_byUnfold().take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }
}
