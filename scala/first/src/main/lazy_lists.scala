
def from(n : Int) : LazyList[Int] = n #:: from(n + 1)

def sieve(numbers : LazyList[Int]) : LazyList[Int] =
    numbers.head #:: sieve(numbers.filter(_ % numbers.head != 0))

def squareRoot(x : Double) : LazyList[Double] =
    def improve(guess : Double) = (guess + x / guess) / 2
    lazy val guesses : LazyList[Double] = 1 #:: guesses.map(improve)
    guesses

def isGoodEnough(target : Double, guess : Double) : Boolean =
    ((target - guess * guess) / target).abs < 0.0001
@main def test =
    println(from(1).take(10).toList)
    println(sieve(from(2)).take(100).toList)
    println(squareRoot(3).filter(isGoodEnough(3, _)).head)