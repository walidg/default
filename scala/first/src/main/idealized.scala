package idealized.scala
abstract class Nat :
    def isZero : Boolean
    def successor : Nat
    def predecessor : Nat
    def + (y : Nat) : Nat
    def - (y : Nat) : Nat

object Zero extends Nat :
    def isZero = true
    def + (y : Nat) = y
    def - (y : Nat) = if y.isZero then this else throw Exception("The result is undefined")
    def successor = Succ(this)
    def predecessor = throw Exception("The result is undefined")
    override def toString = "Zero"

class Succ(n : Nat) extends Nat :
    def isZero = false
    def successor = Succ(this)
    def predecessor = n
    def + (y : Nat) = Succ(n + y)
    def - (y : Nat) =  if y.isZero then this else n - y.predecessor
    override def toString = s"Succ($n)"

@main def test = 
    val one = Succ(Zero) 
    val two = Succ(Succ(Zero))
    assert ((Succ(one) - two).isZero)
    println(s"three is represented as ${one + two}")