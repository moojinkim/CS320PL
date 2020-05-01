package cs320

package object ex05 extends Exercise05 {

  // trait Expr
  // case class Num(num: Int) extends Expr                               // e ::= n
  // case class Add(left: Expr, right: Expr) extends Expr                //     | (e + e)
  // case class Sub(left: Expr, right: Expr) extends Expr                //     | (e - e)
  // case class Val(name: String, value: Expr, body: Expr) extends Expr  //     | {val x=e;e}
  // case class Id(name: String) extends Expr                            //     | x
  // case class App(func: Expr, args: List[Expr]) extends Expr           //     | e(e,...,e)
  // case class Fun(params: List[String], body: Expr) extends Expr       //     | {(x,...,x)=>e}
  // case class Rec(rec: Map[String, Expr]) extends Expr                 //     | {x=e,...,x=e}
  // case class Acc(expr: Expr, name: String) extends Expr               //     | e.x

  // trait Value
  // case class NumV(n: Int) extends Value
  // case class CloV(params: List[String], body: Expr, env: Env) extends Value
  // case class RecV(map: RecMap) extends Value

  // type Env = Map[String, Value]  

  // type RecMap = Map[String, Value]

  def interp(expr: Expr, env: Env): Value = expr match{
    case Num(num) => NumV(num)
    case Add(l,r) => {
      def numVadd(a: Value, b: Value) = (a,b) match{
        case (NumV(a1),NumV(a2)) => NumV(a1+ a2)
        case _ => error("not numV error")
      }
      numVadd(interp(l,env),interp(r,env))
    }
    case Sub(l,r) => {
      def numVsub(a: Value, b: Value) = (a,b) match{
        case (NumV(a1),NumV(a2)) => NumV(a1- a2)
        case _ => error("not numV error")
      }
      numVsub(interp(l,env),interp(r,env))
    }
    case Val(name, value, body) =>
      interp(body, env+(name ->interp(value, env))) 
    case Id(name) => env.getOrElse(name, error("no name"+name))
    case App(func, args) => interp(func, env) match{
      case CloV(params, body, fenv) =>
        val argss=args.map(interp(_,env))
        if (args.length != params.length ) error("wrong arity") else interp(body, fenv++(params zip argss))
      case _ => error("not a function CloV")
    }
    case Fun(params, body) => CloV(params, body, env)
    case Rec(rec) => {
      def mamap ( x: (String,Expr))  = x match{
        case (a, b) => (a, interp(b,env))
      }
      RecV(rec.map(mamap))
    }
    case Acc(expr1, name) => expr1 match {
      case Rec(a) => interp(a.getOrElse(name,error("no such field")),env)
      case _ => error("not a Rec")
    }
    case _ => error("not yet")
  }

  def tests: Unit = {
    test(run("(1+(2+5))"), "8")
    test(run("(1+(5-2))"), "4")
    test(run("{ val xx= 10 ; {val yy=7 ; (xx+yy)} }"), "17")
    test(run("{ (x, y) => (x + y) }(1, 2)"), "3")
    test(run("{ () => (3 + 4) }()"), "7")
    testExc(run("{ (x, y) => (x + y) }(1)"), "wrong arity")
    test(run("{ x = 1, y = 2 }.x"), "1")
    testExc(run("{ x = 1, y = 2 }.z"), "no such field")
    testExc(run("{ x = { y = 1 }.z }"), "no such field")
    test(run("42"), "42")
    test(run("{ x => x }"), "function")
    test(run("{ x = 1 }"), "record")

    /* Write your own tests */
  }
}
