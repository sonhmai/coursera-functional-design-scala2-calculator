package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val bval = b()
      val aval = a()
      val cval = c()
      bval * bval - 4 * aval * cval
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val bval = b()
      val aval = a()
      val deltaSqrt = Math.sqrt(computeDelta(a, b, c)())
      if (deltaSqrt >= 0) {
        val sol1 = (-1 * bval + deltaSqrt) / (2 * aval)
        val sol2 = (-1 * bval - deltaSqrt) / (2 * aval)
        Set(sol1, sol2)
      }
      else Set.empty
    }
  }
}
