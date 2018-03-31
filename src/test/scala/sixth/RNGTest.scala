package sixth

import org.scalatest.FunSpec

class RNGTest extends FunSpec {

  it("6.1") {
    case class FakeRNG(value: Int) extends RNG {
      override def nextInt: (Int, RNG) = (value, this)
    }

    assert(RNG.nonNegativeInt(FakeRNG(1))._1 == 1)
    assert(RNG.nonNegativeInt(FakeRNG(0))._1 == 0)
    assert(RNG.nonNegativeInt(FakeRNG(-1))._1 == 1)
    assert(RNG.nonNegativeInt(FakeRNG(Int.MaxValue))._1 == Int.MaxValue)
    assert(RNG.nonNegativeInt(FakeRNG(Int.MinValue))._1 == 0)
  }
}
