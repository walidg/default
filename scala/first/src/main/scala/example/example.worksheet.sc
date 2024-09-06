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


def operation (reducer : (Int, Int) => Int, init: Int)(from: Int)(to: Int, f: Int => Int) : Int = 
    def loop(a:Int, acc:Int):Int=
        if a > to then acc 
        else loop(a+1, reducer(acc, f(a)))
    loop(from, init)


def sum = operation((x:Int,y:Int) => x+y, 0)
def product = operation((x:Int,y:Int) => x*y, 1)

////// Sum frm 3 to 8 of identities
sum(3)(8, x=>x)

////// Product from 5 to 9 of identities
product(5)(9, x=>x)

def factorial = product(1)
def triangular = sum(1)

//////// 5 factorial
factorial(5, x=>x)

//////// Triangular number for 9
triangular(9, x=>x)

