def f(using n : Int) = ()
given Int = 5


def fibonacci : () => Int =
    var a = 0
    var b = 1 
    def next() = {
        val c = a + b
        a = b
        b = c
        a
    }
    next 

extension(r : => Unit)
    infix def until(cond : => Boolean) : Unit= 
        r
        if(!cond) then r.until(cond) else ()


def repeat(command : => Unit) : Unit = command    
        


@main def test =
    //val f = fibonacci
    //(1 to 20).foreach(i => println(f()))
    var x = 0
    var y = 1
    repeat{
        x = x + 1
        y = y * x
    } until(x == 5)
    println(y)
    
