
package my.packa

extension [T] (e : List[T]) 
    def removeAt(n : Int) : List[T] = e match
        case Nil => Nil
        case x :: xs => if n == 0 then xs else x :: xs.removeAt(n-1) 

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