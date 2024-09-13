def isort(li : List[Int]) : List[Int] =
    li match
        case head :: next => insert(head, isort(next)) 
        case Nil => Nil
def insert(elem : Int, li : List[Int]) : List[Int] =
    li match
        case head :: next => if head < elem then head :: insert(elem, next) else elem :: li
        case Nil => List(elem)
    

@main def test = 
    val x = 1 :: 2 :: 3 :: Nil

    /* Operators that end with : associate to the right 
        1 :: 2 :: 3 :: Nil is equivalent to (1 :: (2 :: (3 :: Nil)))
     */

    assert(!x.isEmpty)
    assert(x.head == 1)
    assert(x.tail.head == 2)

    val li = 10 :: 2 :: 7 :: 9 :: 8 :: 3 :: Nil
    val sorted = isort(li)
    println(sorted)
    assert(sorted.head == 2)