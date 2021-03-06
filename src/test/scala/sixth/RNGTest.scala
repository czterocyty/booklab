package sixth

import org.scalatest.FunSpec
import sixth.RNG.Rand

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

  describe("6.4 list of random") {
    it("empty list") {
      assert(RNG.ints(0)(FakeRNG(0))._1 == List.empty)
    }

    it("1-element list") {
      assert(RNG.ints(1)(FakeRNG(1))._1 == List(1))
    }

    it("3-elements list") {
      assert(RNG.ints(3)(FakeRNG(3))._1 == List(3, 4, 5))
    }
  }

  it("6.4 doubleByMap") {
    assert(RNG.doubleByMap(FakeRNG(0))._1 == 0.0)
    assert(RNG.doubleByMap(FakeRNG(Int.MaxValue))._1 - 1.0 < 0.0000000001d)
    assert(RNG.doubleByMap(FakeRNG(Int.MaxValue / 2))._1 - 0.5d < 0.000000001d)
  }

  it("6.6 map2") {
    def f(a: Double, b: Double): Double = a + b

    assert(RNG.map2(RNG.double, RNG.double)(f)(FakeRNG(Int.MaxValue))._1 - 2.0 < 0.000000000001d)
  }

  it("6.8 flatMap") {
    val (v, _) = RNG.nonNegativeLessThanByFlatMap(10)(FakeRNG(Int.MaxValue))

    assert(v == 9)
  }

  describe("6.9 map and map2 by flatMap") {
    it("map2") {
      def f(a: Double, b: Double): Double = a + b

      assert(RNG.map2_byFlatMap(RNG.double, RNG.double)(f)(FakeRNG(Int.MaxValue))._1 - 2.0 < 0.000000000001d)
    }
  }
}
