package fifth

import org.scalatest.FunSpec

class StreamTest extends FunSpec {

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
}
