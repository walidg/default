abstract class Intset :
    def contains(x: Int) : Boolean
    def include(x : Int) : Intset
    def union (other : Intset) : Intset

object Intset :
    def singleton(x : Int) : Intset = Empty.include(x)

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
    println(Intset.singleton(2).include(5))
    println(Intset.singleton(2).union(Intset.singleton(5)))


