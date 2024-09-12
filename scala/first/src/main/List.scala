package my.types

trait LIST[T] :
    def isEmpty : Boolean
    def head : T
    def tail : LIST[T]


class CONS[T](val head : T, val tail : LIST[T]) extends LIST[T] :
    def isEmpty: Boolean = false

class NIL[T] extends LIST[T] :
    def isEmpty = true
    def head = throw NoSuchMethodException("Nil head does not exist")
    def tail = throw NoSuchMethodException("Nil tail does not exist")


def nth[T] (xs: LIST[T], n:Int) : T = 
    if xs.isEmpty then throw IndexOutOfBoundsException()
    else if n == 0 then xs.head
    else nth(xs.tail, n-1)

@main  def lookup = 
    val l = CONS(11, CONS(25, NIL()))
    assert(nth(l, 0) == 11, "first elem is 11")
    println(s"the second elem of the list is ${nth(l,1)}")