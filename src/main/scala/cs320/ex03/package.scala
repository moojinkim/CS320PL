package cs320

import cs320._

package object ex03 extends Exercise03 {
  // applies a binary numeric function on all combinations of numbers from
  // the two input lists, and return the list of all of the results
  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => Nil
    case l :: rest =>
      def f(r: Int): Int = op(l,r)
      rs.map(f) ++ binOp(op, rest, rs)
  }
  // trait Expr
  // case class Num(nums: List[Int]) extends Expr                       // e ::= n | (n, ..., n)
  // case class Add(left: Expr, right: Expr) extends Expr               //     | (e + e)
  // case class Sub(left: Expr, right: Expr) extends Expr               //     | (e - e)
  // case class Val(name: String, expr: Expr, body: Expr) extends Expr  //     | {val x = e; e}
  // case class Id(id: String) extends Expr                             //     | x
  // case class Min(left: Expr, mid: Expr, right: Expr) extends Expr    //     | min(e, e, e)
  // case class Max(left: Expr, mid: Expr, right: Expr) extends Expr    //     | max(e, e, e)

  def interp(expr: Expr, env: Env): List[Int] =expr match{
    case Num(a) => a
    case Add(left, right) => binOp(_+_, interp(left,env),interp(right,env))
    case Sub(left, right) => binOp(_-_, interp(left,env),interp(right,env))
    case Val(name, expr1, body) => interp(body, env+(name->interp(expr1,env)))
    case Id(id) => env.getOrElse(id, error("no"+id))
    case Min(a,b,c) => binOp(_ min _, binOp(_ min _, interp(a,env),interp(b,env)),interp(c,env))
    case Max(a,b,c) => binOp(_ max _, binOp(_ max _, interp(a,env),interp(b,env)),interp(c,env))    
  }

  def tests: Unit = {
    test(run("(3 + 7)"), List(10))
    test(run("(10 - (3, 5))"), List(7, 5))
    test(run("{ val x = (5 + 5); (x + x) }"), List(20))
    test(run("min(3, 4, 5)"), List(3))
    test(run("max((1 + 2), 4, 5)"), List(5))
    test(run("min((1, 4), (2, 9), 3)"), List(1, 1, 2, 3))
    test(run("max((1, 6), (2, 5), (3, 4))"), List(3, 4, 5, 5, 6, 6, 6, 6))

    /* Write your own tests */
  }
}
