1 + 1

val x = 42

x * x



def product_v1 (f: Int => Int, a: Int, b: Int) : Int = 
    def loop(a:Int, acc:Int):Int=
        if a > b then acc 
        else loop(a+1, acc*f(a))
    loop(a, 1)


def sum_v1 (f: Int => Int, a: Int, b: Int) : Int = 
    def loop(a:Int, acc:Int):Int=
        if a > b then acc 
        else loop(a+1, acc+f(a))
    loop(a, 0)

sum_v1(x => x, 3, 7)
product_v1(x=>x, 1, 5)


def operation (f: Int => Int, reducer : (Int, Int) => Int, init: Int)(from: Int, to: Int) : Int = 
    def loop(a:Int, acc:Int):Int=
        if a > to then acc 
        else loop(a+1, reducer(acc, f(a)))
    loop(from, init)


def sum(f: Int => Int) = operation(f, (x:Int,y:Int) => x+y, 0)
def product(f: Int => Int) = operation(f, (x:Int,y:Int) => x*y, 1)

////// Sum from 3 to 8 of identities
sum(x=>x)(3, 8)

////// Product from 5 to 9 of identities
product(x=>x)(5, 9)

def factorial(n : Int) = product(x=>x)(1, n)
def triangular(n : Int) = sum(x=>x)(1, n)

//////// 5 factorial
factorial(5)

//////// Triangular number for 9
triangular(9)

sum(triangular)(2, 5)

