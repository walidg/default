def gcd(x : Int, y : Int) : Int = 
    if x % y == 0 then y else gcd(y, x % y)

gcd(1, 5)
gcd(10, 6)

class Rational(x : Int, y : Int) :
    require(y>0, s"Denominator must be positive, was $x/$y")
    def this(x : Int) = this(x, 1)
    def numer = x / gcd(x.abs,y)
    def denom = y / gcd(x.abs,y)
    def add(r: Rational) = Rational(numer * r.denom + r.numer * denom, denom * r.denom)
    def mul(r: Rational) = Rational(numer * r.numer, denom * r.denom)
    def neg = Rational(-numer, denom)
    def sub(r: Rational) = add(r.neg)
    def less(r:Rational) = numer * r.denom < r.numer * denom
    def max(r:Rational) = if less(r) then r else this
    override def toString = s"$numer/$denom"
end Rational

extension(s:Rational)
    def min(r:Rational): Rational = if s.less(r) then s else r
    def abs : Rational = Rational(s.numer.abs, s.denom)


Rational(2)

val r = Rational(1, 5).add(Rational(2, 7)).sub(Rational(19, 35))
assert(r.numer == -2 , "result numer must be -2")


assert(Rational(-5, 2).abs.numer == 5)
Rational(1, -2)
