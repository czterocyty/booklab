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
}
