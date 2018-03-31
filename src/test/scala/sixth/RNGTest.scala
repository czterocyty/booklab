package sixth

import org.scalatest.FunSpec

class RNGTest extends FunSpec {

  case class FakeRNG(value: Int) extends RNG {
    override def nextInt: (Int, RNG) = (value, FakeRNG(value+1))
  }

  it("6.1") {
    assert(RNG.nonNegativeInt(FakeRNG(1))._1 == 1)
    assert(RNG.nonNegativeInt(FakeRNG(0))._1 == 0)
    assert(RNG.nonNegativeInt(FakeRNG(-1))._1 == 1)
    assert(RNG.nonNegativeInt(FakeRNG(Int.MaxValue))._1 == Int.MaxValue)
    assert(RNG.nonNegativeInt(FakeRNG(Int.MinValue))._1 == Int.MaxValue)
  }

  it("6.2 double") {
    assert(RNG.double(FakeRNG(0))._1 == 0.0)
    assert(RNG.double(FakeRNG(Int.MaxValue))._1 - 1.0 < 0.0000000001d)
    assert(RNG.double(FakeRNG(Int.MaxValue / 2))._1 - 0.5d < 0.000000001d)
  }
}
