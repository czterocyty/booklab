package fourth

import org.scalatest.FunSpec

class EitherTest extends FunSpec {

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

  describe("sequence") {
    it("only Rights") {
      assert(Either.sequence(List(Right("A"), Right("B"))) == Right(List("A", "B")))
    }

    it("Left inside") {
      assert(Either.sequence(List(Right("A"), Left(3))) == Left(3))
    }
  }

  describe("traverse") {
    def f(s: String): Either[Int, String] = {
      if (s.length > 1) { Right(s) } else { Left(1) }
    }

    it("only Rights") {
      assert(Either.traverse(List("ac", "bc"))(f) == Right(List("ac", "bc")))
    }

    it("with error") {
      assert(Either.traverse(List("ac", "a"))(f) == Left(1))
    }
  }

}
