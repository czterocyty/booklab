package sixth

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5deece66dl + 0xbl) & 0xffffffffffffffl
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val t = rng.nextInt
    t._1 match {
      case Int.MinValue => nonNegativeInt(t._2)
      case v => (math.abs(v), t._2)
    }
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val t = nonNegativeInt(rng)
    (t._1.toDouble / (Int.MaxValue + 1l), t._2)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val it = nonNegativeInt(rng)
    val dt = double(it._2)
    ((it._1, dt._1), dt._2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val dt = double(rng)
    val it = nonNegativeInt(dt._2)
    ((dt._1, it._1), it._2)
  }
  def threeDouble(rng: RNG): ((Double, Double, Double), RNG) = {
    val d1 = double(rng)
    val d2 = double(d1._2)
    val d3 = double(d2._2)
    ((d1._1, d2._1, d3._1), d3._2)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count < 1) {
      (List(), rng)
    } else {
      val t = rng.nextInt
      val x = ints(count-1)(t._2)
      (t._1 :: x._1, x._2)
    }

  }
}
