def abs(a : Double) = if a >= 0 then a else -a
def isCloseEnough(v1 : Double, v2 : Double) : Boolean = abs((v1 - v2) / v1) < 0.00001
def averageDamp(f : Double => Double) (x : Double) = (x + f(x)) / 2

def fixedPoint(f : Double => Double) (guess : Double) : Double = 
    val next = f(guess)
    if isCloseEnough(next, guess) then next
    else fixedPoint(f)(next)

def sqrt(x : Double) = fixedPoint(averageDamp(y => x / y))(1)

sqrt(2)
sqrt(3)