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
}
