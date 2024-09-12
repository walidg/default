abstract class Intset :
    def contains(x: Int) : Boolean
    def include(x : Int) : Intset
    def union (other : Intset) : Intset

object Intset :
    def apply() : Intset = Empty
    def apply(x : Int) : Intset = apply().include(x)
    def apply(x : Int, y : Int) : Intset = apply(x).include(y)
    def apply(x : Int, y : Int, z : Int) : Intset = apply(x,y).include(z)

object Empty extends Intset :
    def contains(x: Int): Boolean = false
    def include(x: Int): Intset = NonEmpty(x, Empty, Empty)
    def union(other: Intset): Intset = other
    override def toString(): String = "E"

class NonEmpty(root : Int, left : Intset, right : Intset) extends Intset :
    def contains(x: Int): Boolean = 
        if x < root then left.contains(x)
        else if x > root then right.contains(x)
        else true
    def include(x: Int): Intset = 
        if x < root then NonEmpty(root, left.include(x), right)
        else if x > root then NonEmpty(root, left, right.include(x))
        else this
    def union(other: Intset): Intset = left.union(right).union(other).include(root)
    override def toString(): String = s"$left+$root+$right" 
    
object Main :
  @main def helloworld = 
    println(Intset(2).include(5))
    println(Intset(2).union(Intset(5)))
    val is = Intset(5, 6)
    val xs = Intset(1,9,17)
    assert (is.contains(5) && is.contains(6))
    assert (xs.contains(1) && xs.contains(9) && xs.contains(17))


