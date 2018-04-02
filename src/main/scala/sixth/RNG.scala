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

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

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

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // 6.5
  def doubleByMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue + 1l))

  // 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    rng => {
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n-1) - mod >= 0) {
        (mod, rng2)
      } else {
        nonNegativeLessThan(n)(rng)
      }
    }
  }

  // 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      val (v, rng3) = g(a)(rng2)
      (v, rng3)
    }
  }

  def nonNegativeLessThanByFlatMap(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i => {
      rng => {
        val mod = i % n
        if (i + (n-1) - mod >= 0) {
          (mod, rng)
        } else {
          nonNegativeLessThanByFlatMap(n)(rng)
        }
      }
    })
  }

  // 6.9
  def map_byFlatMap[A, B](action: Rand[A])(f: A => B): Rand[B] = {
    flatMap(action)(a => {
      rng => {
        (f(a), rng)
      }
    })
  }

  def map2_byFlatMap[A, B, C](actionA: Rand[A], actionB: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(actionA)(a => flatMap(actionB)(b => {
      rng => {
        (f(a, b), rng)
      }
    }))
  }
}
