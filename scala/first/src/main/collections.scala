def combinations(n : Int, m : Int) : Seq[(Int, Int)] =
    require(n > 0 && m > 0, "input parameters must be positive ($n, $m)")
    (1 to n).flatMap(x => (1 to m).map(y => (x, y)))

// Scalar product of two vectors
def scalarProduct(v1 : Vector[Double], v2 : Vector[Double]) : Double =
    v1.zip(v2).map(_ * _).sum

def scalarProduct2(v1 : Vector[Double], v2 : Vector[Double]) : Double =
    (for (x, y) <- v1.zip(v2) yield x * y).sum

def isPrime(n : Int) : Boolean = 
    require(n > 0, "input parameter $n must be positive")
    (2 to (n - 1)).forall(n % _ != 0)

    
/* 
    Define all pairs (i, j) such that 1<= j < i < n and i+j is prime
    1. Functional way
 */
def primePairs1(n : Int) : Seq[(Int, Int)] = 
    (1 until n).flatMap(x => (1 until x).map(y => (x, y)))
    .filter(yx => isPrime(yx._1 + yx._2))
   
/* 
    Define all pairs (i, j) such that 1<= j < i < n and i+j is prime
    2. For-Expression way
 */
def primePairs2(n : Int) : Seq[(Int, Int)] = 
    for
        i <- 1 until n
        j <- 1 until i
        if isPrime(i +j)
    yield (i, j)

@main def test =
    assert(scalarProduct(Vector(1, 2, 3), Vector(5, 6, 7)) == 38)
    assert(isPrime(53))
    assert(isPrime(2))
    assert(primePairs2(7) == primePairs1((7)))
    assert(scalarProduct(Vector(1, 2, 3), Vector(5, 6, 7)) == scalarProduct2(Vector(1, 2, 3), Vector(5, 6, 7)))