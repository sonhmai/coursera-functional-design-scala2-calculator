package calculator

class PolynomialSuite extends munit.FunSuite {

  import Polynomial._

  import Ordering.Double.TotalOrdering

  def kindaEqual(a: Double, b: Double): Boolean =
    a > b - 1e-5 && a < b + 1e-5

  test("computeDelta test") {
    val (a, b, c) = (Var(1.0), Var(4.0), Var(1.0))
    val result = computeDelta(a, b, c)

    assert(kindaEqual(result(), 12.0))
    a() = -5.3
    assert(kindaEqual(result(), 37.2))
    c() = -123.456
    assert(kindaEqual(result(), -2601.2672))
  }

  test("computeSolutions test") {
    val (a, b, c) = (Var(1.0), Var(4.0), Var(1.0))
    val delta = Var(12.0)
    val result = computeSolutions(a, b, c, delta)

    assertEquals(result().size, 2)
    assert(kindaEqual(result().min, -3.732050807568877))
    assert(kindaEqual(result().max, -0.2679491924311228))

    a() = -5.3
    delta() = 37.2
    assertEquals(result().size, 2)
    assert(kindaEqual(result().min, -0.1980358747915814))
    assert(kindaEqual(result().max, 0.9527528559236569))

    c() = -123.456
    delta() = -2601.2672
    assertEquals(result().size, 0)
  }
}

