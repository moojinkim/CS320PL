package cs320

package object ex02 extends Exercise02 {
  // Problem 1
  def freeIds(expr: Expr): Set[String] = ???

  // Problem 2
  def bindingIds(expr: Expr): Set[String] = ???

  // Problem 3
  def boundIds(expr: Expr): Set[String] = ???

  // Tests
  def tests: Unit = {
    test(freeIds(Expr("{ val x = 1; (x + y) }")), Set("y"))
    test(freeIds(Expr("{ val z = 2; 1 }")), Set())
    test(bindingIds(Expr("{ val x = 1; (x + y) }")), Set("x"))
    test(bindingIds(Expr("{ val z = 2; 1 }")), Set("z"))
    test(boundIds(Expr("{ val x = 1; (x + y) }")), Set("x"))
    test(boundIds(Expr("{ val z = 2; 1 }")), Set())

    /* Write your own tests */
  }
}
