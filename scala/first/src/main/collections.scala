def combinations(n : Int, m : Int) : Seq[(Int, Int)] =
    require(n > 0 && m > 0, "input parameters must be positive ($n, $m)")
    (1 to n).flatMap(x => (1 to m).map(y => (x, y)))

def scalarProduct(v1 : Vector[Double], v2 : Vector[Double]) : Double =
    v1.zip(v2).map(_ * _).sum

def isPrime(n : Int) : Boolean = 
    require(n > 0, "input parameter $n must be positive")
    (2 to (n - 1)).forall(n % _ != 0)

@main def test =
    assert(scalarProduct(Vector(1, 2, 3), Vector(5, 6, 7)) == 38)
    assert(isPrime(53))
    assert(isPrime(2))