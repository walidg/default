def gcd(x : Int, y : Int) : Int = 
    if x % y == 0 then y else gcd(y, x % y)

gcd(1, 5)
gcd(10, 6)

class Rational(x : Int, y : Int) :
    def numer = x / gcd(x,y)
    def denom = y / gcd(x,y)
    def add(r: Rational) = Rational(numer * r.denom + r.numer * denom, denom * r.denom)
    def mul(r: Rational) = Rational(numer * r.numer, denom * r.denom)
    def neg = Rational(-numer, denom)
    def sub(r: Rational) = add(r.neg)
    override def toString = s"$numer/$denom"

Rational(1, 5).add(Rational(2, 7)).sub(Rational(15, 35))