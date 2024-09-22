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


class Polynom(nonZeroElements : Map[Int, Double]) :
    def this(bindings : (Int, Double)* ) = this(bindings.toMap)

    val terms = nonZeroElements.withDefaultValue(0.0)

    def +(other: Polynom) : Polynom = Polynom(terms ++ other.terms.map((exp, coeff) => (exp, terms(exp) + coeff)))

    def *(other: Polynom) : Polynom = 
        (for (exp, coeff) <- other.terms
        yield Polynom(terms.map((x, y) => (x + exp, y * coeff))))
        .reduce(_ + _)
    
    def *(other: Double) : Polynom = Polynom(terms.map((exp, coeff) => (exp, coeff * other)))

    override def toString(): String = 
        val termsStrings = 
            for (exp, coeff) <- terms.toList.sorted.reverse
            yield 
                val exponent = exp match
                    case 0 => ""
                    case 1 => "x"
                    case _ => s"x^$exp"
                s"$coeff$exponent"
        if terms.isEmpty then "0"
        else termsStrings.init.foldRight(s"${termsStrings.last}") ((x, acc) => acc.toList match
            case '-' :: xs => s"$x - ${acc.substring(1)}" 
            case _ => s"$x + $acc"
            )

    override def equals(x: Any): Boolean = x match
        case p : Polynom => checkEquals(p)
        case _ => false

    private def checkEquals(other : Polynom) : Boolean = 
        terms.size == other.terms.size 
        && terms.toList.sorted.zip(other.terms.toList.sorted).forall((kv1, kv2) => kv1._1 == kv2._1 && kv1._2 == kv2._2)
    

extension (e : Double) 
    def *(p : Polynom) = p * e


@main def test =
    assert(scalarProduct(Vector(1, 2, 3), Vector(5, 6, 7)) == 38)
    assert(isPrime(53))
    assert(isPrime(2))
    assert(primePairs2(7) == primePairs1((7)))
    assert(scalarProduct(Vector(1, 2, 3), Vector(5, 6, 7)) == scalarProduct2(Vector(1, 2, 3), Vector(5, 6, 7)))

    val p1 = Polynom(0 -> 5, 1 -> -2, 3 -> -2)
    val p2 = Polynom(1 -> 3, 2 -> 3)
    val product = Polynom(5 -> -6, 1 -> 15, 2 -> 9, 3 -> -6, 4 -> -6)

    assert(product == p1 * p2)
    assert(product != p1)
    assert(2 * p2 == p2 * 2)