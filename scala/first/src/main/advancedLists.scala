
package my.packa

extension [T] (e : List[T]) 
    def removeAt(n : Int) : List[T] = e match
        case Nil => Nil
        case x :: xs => if n == 0 then xs else x :: xs.removeAt(n-1) 
    def insertionSort : List[T] = e match
        case Nil => Nil
        case x :: xs => insert(x, xs.insertionSort)

private def insert[T]( i : T, xs : List[T]) : List[T] = xs match
    case Nil => List(i)
    case y :: ys => if compareTo(i, y) then i :: xs else y :: insert(i, ys)
private def compareTo(a1 : Any, a2 : Any) : Boolean = (a1, a2) match
    case (z1 : Int, z2 : Int) => z1 < z2
    case (z1 : Double, z2 : Double) => z1 < z2
    case (z1 : Char, z2 : Char) => z1 < z2
    case _ => ???
    

def flatten(a : Any) : List[Any] = a match
    case l : List[Any] => flatten(l) ++ Nil
    case i : Any => i :: Nil

def flatten(xs : List[Any]) : List[Any] = xs match
    case Nil => Nil
    case y :: ys => flatten(y) ++ flatten(ys)


@main def test =
    val l = List(1, 2, 3, 4)
    assert(l.removeAt(-1) == l)
    assert(l.removeAt(0) == List(2,3,4))
    assert(l.removeAt(1) == List(1,3,4))
    assert(l.removeAt(2) == List(1,2,4))
    assert(l.removeAt(3) == List(1,2,3))
    assert(l.removeAt(4) == l)
    assert(flatten( List(
        List(List(1,3, List(5))),
         7,
        List(10, 8, List(List(9, 17)))
        )) ==
         List(1, 3, 5, 7, 10, 8, 9, 17)   )

    assert(List(5, 4, 7, 2, 3, 9, 6, 1, 5).insertionSort == List(1, 2, 3 ,4 , 5, 5, 6, 7, 9))

    assert(List(5.0, 4.2, 7, 2, 3, 9, 6, 1, 5).insertionSort == List(1, 2, 3 ,4.2 , 5, 5, 6, 7, 9))

    assert("feairpji".toList.insertionSort == "aefiijpr".toList)