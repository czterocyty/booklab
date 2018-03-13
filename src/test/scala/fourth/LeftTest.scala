package fourth

import org.scalatest.FunSpec

class LeftTest extends FunSpec {

  describe("Left") {
    it("map") {
      assert(Left("error").map(v => v) == Left("error"))
    }

    it("map2") {
      assert(Left("error").map2(Right("error"))((_, b) => b) == Left("error"))
    }
  }

  describe("Right") {
    it("map") {
      assert(Right("ok").map(_.length) == Right(2))
    }

    it("map2") {
      assert(Right("ok").map2(Right("ok"))(_ + _) == Right("okok"))
    }
  }

}
