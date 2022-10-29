package calculator

class CalculatorSuite extends munit.FunSuite {

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

  /****************
   ** CALCULATOR **
   ****************/

   import Calculator._

  // test cases for calculator
  test("Self dependency") {
    val input = Map("a" -> Signal[Expr](Plus(Ref("a"), Literal(1))),
      "d" -> Signal[Expr](Minus(Literal(5), Literal(3))))
    val output = computeValues(input)
    val check = output("a")().isNaN
    assert(check, " - Your implementation should return NaN when the expression for a variable " +
      "references the variable itself")

    val checkRes = (output("d")() == 2)
    assert(checkRes, " - Your implementation should return a valid result for variables " +
      "that do not refer to themselves")
  }

  test("Cyclic dependencies should result in NaN") {
    val input = Map("a" -> Signal[Expr](Plus(Ref("b"), Literal(1))),
      "b" -> Signal[Expr](Divide(Ref("c"), Ref("d"))),
      "c" -> Signal[Expr](Times(Literal(5), Ref("a"))),
      "d" -> Signal[Expr](Minus(Literal(5), Literal(3))))
    val output = computeValues(input)
    val checkNaNs = output("a")().isNaN && output("b")().isNaN && output("c")().isNaN
    assert(checkNaNs, " - Your implementation should return NaN for variables that have cyclic dependencies")

    val checkRes = (output("d")() == 2)
    assert(checkRes, " - Your implementation should return a valid result for variables " +
      "that do not have any cyclic dependency")
  }

  test("Referencing an unknown variable should result in NaN") {
    val input = Map("a" -> Signal[Expr](Plus(Ref("b"), Literal(1))),
      "d" -> Signal[Expr](Minus(Literal(5), Literal(3))))
    val output = computeValues(input)
    val check = output("a")().isNaN
    assert(check, " - Your implementation should return NaN on evaluating an expression that " +
      "references an unknown variable")

    val checkRes = (output("d")() == 2)
    assert(checkRes, " - Your implementation should return a valid result for variables " +
      "that do not reference unknown variables")
  }

  test("Expressions corresponding to every variable should be correctly evaluated") {
    val aexpr = Var[Expr](Plus(Ref("b"), Literal(1)))
    val bexpr = Var[Expr](Times(Ref("c"), Ref("d")))
    val cexpr = Var[Expr](Plus(Literal(5), Ref("d")))
    val dexpr = Var[Expr](Minus(Literal(4), Literal(3)))
    val input = Map("a" -> aexpr, "b" -> bexpr, "c" -> cexpr, "d" -> dexpr)
    val output = computeValues(input)
    var ares = output("a")() == 7
    assert(ares, " - Value of `a` is incorrect!")
    var bres = output("b")() == 6
    assert(bres, " - Value of `b` is incorrect!")
    var cres = output("c")() == 6
    assert(cres, " - Value of `c` is incorrect!")
    var dres = output("d")() == 1
    assert(dres, " - Value of `d` is incorrect!")

    // update `d`
    dexpr() = Plus(Literal(4), Literal(3))
    ares = output("a")() == 85
    assert(ares, " - Value of `a` is incorrect after updating expression `d`!")
    bres = output("b")() == 84
    assert(bres, " - Value of `b` is incorrect after updating expression `d`!")
    cres = output("c")() == 12
    assert(cres, " - Value of `c` is incorrect after updating expression `d`!")
    dres = output("d")() == 7
    assert(dres, " - Value of `d` is incorrect after updating expression `d`!")
  }

  test("When an expression changes, other variables that depend on it should be recomputed") {
    val aexpr = Var[Expr](Plus(Ref("b"), Literal(1)))
    val bexpr = Var[Expr](Times(Ref("c"), Ref("d")))
    val cexpr = Var[Expr](Plus(Literal(5), Ref("d")))
    val dexpr = Var[Expr](Minus(Literal(4), Literal(3)))
    val input = Map("a" -> aexpr, "b" -> bexpr, "c" -> cexpr, "d" -> dexpr)
    val output = computeValues(input)

    val oldvals = output.map {
      case (k, v) => (k -> v())
    }
    cexpr() = Plus(Literal(4), Literal(3))
    val checkRes = (output("a")() != oldvals("a")) && (output("b")() != oldvals("b")) &&
      (output("c")() != oldvals("c")) && (output("d")() == oldvals("d"))
    assert(checkRes, " - Your implementation should update the dependent values " +
      "when an expression changes")
  }

  test("If b previously depended on a, but no longer does, then it should not be recomputed anymore when a changes") {
    val aexpr = Var[Expr](Minus(Literal(4), Literal(3)))
    val bexpr = Var[Expr](Plus(Ref("a"), Literal(1)))
    val input = Map("a" -> aexpr, "b" -> bexpr)
    val output = computeValues(input)

    val oldvals = output.map {
      case (k, v) => (k -> v())
    }
    bexpr() = Literal(1)

    var bchanged = -1
    val depSignal = Signal {
      output("b")()
      bchanged += 1
    }
    aexpr() = Literal(5)
    val checkRes = bexpr() == Literal(1)
    assert(checkRes, " - The value of `b` should be 1")
    assert(bchanged == 0, " - Signal `b` should not be recomputed")
  }

  test("When an expression changes, *only* the variables that depend on it should be recomputed") {
    val aexpr = Var[Expr](Plus(Ref("b"), Literal(1)))
    val bexpr = Var[Expr](Times(Literal(5), Ref("d")))
    val cexpr = Var[Expr](Plus(Literal(5), Ref("d")))
    val dexpr = Var[Expr](Minus(Literal(4), Literal(3)))
    val input = Map("a" -> aexpr, "b" -> bexpr, "c" -> cexpr, "d" -> dexpr)
    val output = computeValues(input)

    var accessMap = Map[String, Int]()
    def updateMap(key: String): Unit = {
      if (accessMap.contains(key))
        accessMap = accessMap.updated(key, accessMap(key) + 1)
      else accessMap += (key -> 0)
    }

    val depSignals = output.map {
      case (k, v) => (k -> Signal {
        v()
        updateMap(k)
      })
    }
    cexpr() = Plus(Literal(4), Literal(3))
    val checkRes = accessMap.filterNot(x => x._1 == "c").forall(x => x._2 == 0)
    assert(checkRes, " - Your implementation should update only the dependent values " +
      "when an expression changes")
  }
/*++++++*/

  import scala.concurrent.duration._
  override val munitTimeout = 10.seconds
}

