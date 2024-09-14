import java.time.LocalDate
trait Expr
object Expr :
    case class Number(aNumber:Int) extends Expr
    case class Sum(anExpr1 : Expr, anExpr2 : Expr) extends Expr
    case class Prod(anExpr1 : Expr, anExpr2 : Expr) extends Expr
    case class Var(name : String, e : Expr) extends Expr
end Expr


/*   The above algebraic data types "ADT" are equivalent to the enum hereafter 
 */
enum Expr2:
    case Number(aNumber:Int)
    case Sum(anExpr1 : Expr2, anExpr2 : Expr2)
    case Prod(anExpr1 : Expr2, anExpr2 : Expr2)
    case Var(name : String, e : Expr2)
end Expr2

def eval(e : Expr2): Int = e match
    case Expr2.Number(n) => n
    case Expr2.Sum(e1, e2) => eval(e1) + eval(e2)
    case Expr2.Prod(e1, e2) => eval(e1) * eval(e2)
    case Expr2.Var(_, e) => eval(e)  

/* 
    enum with simple cases or constants 
*/
enum Color :
    case Blue, Red, Green

/* 
    enum can take parameters and define methods
    ordinal and values are only available for simple cases 
    The enum code below is expanded by the compiler to roghly the following
    abstract class Direction(val dx : Int, val dy : Int) :
        def turnLeft = Direction.values((ordinal + 1) % 4)
    object Direction :
        val Right = new Direction(1, 0) {}
        val Up = new Direction(0, 1) {}
        val Left = new Direction(-1, 0) {}
        val Down = new Direction(0, -1) {} 
    end Direction
*/
enum Direction(val dx : Int, val dy : Int) :
    case Right extends Direction(1, 0)
    case Up extends Direction(0, 1)
    case Left extends Direction(-1, 0)
    case Down extends Direction(0, -1)

    def leftTurn = Direction.values((ordinal + 1) % 4)
end Direction

enum PaymentMethod :
    case CreditCard(kind : Card, holder : String, expiry : LocalDate)
    case Paypal(email : String)
    case Cash
enum Card :
    case Visa, MasterCard, Amex

@main def test = 

    import Expr2._
    assert(eval(Number(5)) == 5)
    assert(eval(Var("x", Number(8))) == 8)
    assert(true)
    val right = Direction.Right
    assert(right.dx == 1 && right.dy == 0)
    assert(right.leftTurn.dy == 1)
    assert(Direction.valueOf("Left").dx == -1)