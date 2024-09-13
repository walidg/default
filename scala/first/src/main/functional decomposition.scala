
trait Expr :
    def eval : Int = this match
        case Number(n) => n
        case Sum(e1, e2) => e1.eval + e2.eval
        case Prod(e1, e2) => e1.eval * e2.eval
        case Var(_, e) => e.eval  

    def show : String = this match
        case Number(n) => n.toString
        case Sum(e1, e2) => s"${e1.show} + ${e2.show}"
        case Prod(e1 : Sum, e2 : Sum) => s"(${e1.show}) * (${e2.show})"
        case Prod(e1 : Sum, e2) => s"(${e1.show}) * ${e2.show}"
        case Prod(e1, e2 : Sum) => s"${e1.show} * (${e2.show})"
        case Prod(e1, e2) => s"${e1.show} * ${e2.show}"
        case Var(n, _) => n
    
case class Number (aNumber:Int) extends Expr
case class Sum(anExpr1 : Expr, anExpr2 : Expr) extends Expr
case class Prod(anExpr1 : Expr, anExpr2 : Expr) extends Expr
case class Var(name : String, e : Expr) extends Expr

@main def test =
    val five = Number(5)
    val one = Number(1)
    val seven = Number(7)
    val varx = Var("x", Number(20))
    assert(five.eval == 5)
    assert(Sum(one, seven).eval == 8)
    assert(five.show == "5")
    assert(Sum(five, seven).show == "5 + 7")
    assert(Prod(five, seven).eval == 35)
    assert(Prod(Sum(five, one), seven).eval == 42)
    assert(Prod(Sum(five, one), seven).show == "(5 + 1) * 7")
    assert(Sum(Prod(five, one), seven).eval == 12)
    assert(Sum(Prod(five, one), seven).show == "5 * 1 + 7")
    assert(Prod(Sum(five, varx), seven).eval == 175)
    assert(Prod(Sum(five, varx), seven).show == "(5 + x) * 7")
    assert(Sum(Prod(five, varx), seven).eval == 107)
    assert(Sum(Prod(five, varx), seven).show == "5 * x + 7")
     
  
