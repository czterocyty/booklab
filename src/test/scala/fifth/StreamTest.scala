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
        assert(Stream("A", "B", "C").take(2) == List("A", "B"))
      }

      it("n greather than stream size") {
        assert(Stream("A").take(2) == List("A"))
      }

      it("Empty") {
        assert(Stream.empty.take(1) == List.empty)
      }
    }
    
    describe("drop") {
      it("3-element stream") {
        assert(Stream("A", "B", "C").drop(2) == List("C"))
      }

      it("n greather than stream size") {
        assert(Stream("A").drop(2) == List())
      }

      it("Empty") {
        assert(Stream.empty.drop(1) == List.empty)
      }
      
    }
  }

}
