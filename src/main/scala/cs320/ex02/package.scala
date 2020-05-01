package cs320

package object ex02 extends Exercise02 {
  // trait Expr
  // case class Num(num: Int) extends Expr                             // e ::= n
  // case class Add(left: Expr, right: Expr) extends Expr              //     | (e + e)
  // case class Sub(left: Expr, right: Expr) extends Expr              //     | (e - e)
  // case class Val(name: String, expr: Expr, body: Expr) extends Expr //     | {val x = e; e}
  // case class Id(id: String) extends Expr                            //     | x

  // Problem 1
  def freeIds(expr: Expr): Set[String] = {
    def frees(e:Expr, seta: Set[String]) : Set[String]={
      e match {
        case Num(a) => Set()
        case Add(a,b) => frees(a,seta) ++frees(b,seta)
        case Sub(a,b) => frees(a,seta) ++frees(b,seta)  
        case Val(a,b,c) => frees(b,seta) ++frees(c,seta++Set(a))     
        case Id(a) => if (seta contains a) Set() else Set(a)
      }
    }
    frees(expr, Set())
  }

  // Problem 2
  def bindingIds(expr: Expr): Set[String] = {
    def binds(e:Expr, seta: Set[String]) : Set[String]={
      e match {
        case Num(a) => Set()
        case Add(a,b) => binds(a,seta) ++binds(b,seta)
        case Sub(a,b) => binds(a,seta) ++binds(b,seta)  
        case Val(a,b,c) => Set(a) ++binds(b,seta) ++binds(c,seta)     
        case Id(a) => Set()
      }
    }
    binds(expr, Set())
  }

  // Problem 3
  def boundIds(expr: Expr): Set[String] = {
    def bound(e:Expr, seta: Set[String]) : Set[String]={
      e match {
        case Num(a) => Set()
        case Add(a,b) => bound(a,seta) ++bound(b,seta)
        case Sub(a,b) => bound(a,seta) ++bound(b,seta)  
        case Val(a,b,c) => bound(b,seta+a) ++bound(c,seta+a)     
        case Id(a) => if (seta contains a) Set(a) else Set()
      }
    }
    bound(expr, Set())
  }

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
