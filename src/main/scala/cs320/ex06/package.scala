package cs320

package object ex06 extends Exercise06 {
  def numVop(op: (Int, Int) => Int): (Value, Value) => NumV = (_, _) match {
    case (NumV(x), NumV(y)) => NumV(op(x, y))
    case (x, y) => error(s"not both numbers: $x, $y")
  }
  val numVAdd = numVop(_ + _)
  val numVSub = numVop(_ - _)

  // trait Expr
  // case class Num(num: Int) extends Expr                                // e ::= n
  // case class Add(left: Expr, right: Expr) extends Expr                 //     | (e+e)
  // case class Sub(left: Expr, right: Expr) extends Expr                 //     | (e-e)
  // case class Id(name: String) extends Expr                             //     | x
  // case class Fun(param: String, body: Expr) extends Expr               //     | {x=>e}
  // case class App(fun: Expr, arg: Expr) extends Expr                    //     | e(e)
  // case class NewBox(expr: Expr) extends Expr                           //     | Box(e)
  // case class SetBox(box: Expr, expr: Expr) extends Expr                //     | e.set(e)
  // case class OpenBox(box: Expr) extends Expr                           //     | e.get
  // case class Seqn(left: Expr, right: List[Expr]) extends Expr          //     | {e;...;e}
  // case class Rec(fields: List[(String, Expr)]) extends Expr            //     | {x=e,...,x=e}
  // case class Get(record: Expr, field: String) extends Expr             //     | e.x
  // case class Set(record: Expr, field: String, expr: Expr) extends Expr //     | {e.x=e}

  // // environment
  // type Addr = Int
  // type Env = Map[String, Value]
  // type Sto = Map[Addr, Value]

  // // value type
  // trait Value
  // case class NumV(n: Int) extends Value
  // case class CloV(param: String, body: Expr, env: Env) extends Value
  // case class BoxV(addr: Addr) extends Value
  // case class RecV(fields: Map[String, Addr]) extends Value
  def malloc(s: Sto) : Addr =
    maxAddr(s)+1
  def maxAddr(s: Sto) : Addr =
    s.keySet.+(0).max


  def interp(e: Expr, env: Env, sto: Sto): (Value, Sto) = e match {
    case Num(n) => (NumV(n), sto)
    case Add(l, r) =>
      val (lv, ls) = interp(l, env, sto)
      val (rv, rs) = interp(r, env, ls)
      (numVAdd(lv, rv), rs)
    case Sub(l, r) =>
      val (lv, ls) = interp(l, env, sto)
      val (rv, rs) = interp(r, env, ls)
      (numVSub(lv, rv), rs)
    case Id(x) =>
      val v = env.getOrElse(x, error(s"free identifier: $x"))
      (v, sto)
    case Fun(p, b) =>
      val cloV = CloV(p, b, env)
      (cloV, sto)
    case App(fun, arg) => {
      val (lv,ls)= interp(fun, env, sto)
      lv match{
        case CloV(param, body, fenv) => {
          val (rv,rs)=interp(arg, env, ls)
          val (v, s)=interp(body, fenv+(param->rv),rs)
          (v,s)
        }
        case _ => error("not a function")
      }
    }
    case NewBox(e1) =>{
      val (v2,s2) = interp(e1, env, sto) 
      val addr = malloc(s2) //give address
      (BoxV(addr),s2+(addr -> v2))
    }
    case SetBox(box, e1) => {
      interp(box,env,sto) match {
        case (BoxV(b1),s2) =>{
          val (v3,s3) = interp(e1, env, s2)
          (v3,s3+(b1->v3))
        }
        case _ => error("not box")
      }
    }
    case OpenBox(box) => interp(box, env, sto) match {
      case (BoxV(b1),s1) => {
        val v=s1.getOrElse(b1,error("invalid address"))
        (v,s1)
      }
      case _ => error("not a box")
    }
    case Seqn(left, right) =>{
      val (v1,s1)=interp(left, env, sto)
      right match{
        case e1::e2 => interp(Seqn(e1,e2),env,s1)
        case Nil => (v1,s1)
      }
    }
  }

  def tests: Unit = {
    test(run("{ b => b.get }(Box(10))"), "10")
    test(run("{b => {b.set(15);b.get}}(Box(10))"),"15")

    test(run("""{
                  b => {
                    b.set((2 + b.get));
                    b.set((3 + b.get));
                    b.set((4 + b.get));
                    b.get
                  }
                }(Box(1))"""), "10")
    // testExc(run("{ x = 1 }.y"), "no such field")
    // test(run("""{
    //               r => {
    //                 { r.x = 5 };
    //                 r.x
    //               }
    //             }({ x = 1 })"""), "5")
    test(run("42"), "42")
    test(run("{ x => x }"), "function")
    test(run("Box(1)"), "box")
    // test(run("{}"), "record")

    /* Write your own tests */
  }
}
