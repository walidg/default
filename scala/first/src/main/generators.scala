/*  This file defines random generators for
    Integers, Booleans, Pairs, Single, Range
    OneOf, Lists, Trees
 */
package my.generators


trait Generator[+T]:
    def generate : T

extension [T, S](g: Generator[T])
    def map(f: T => S) = new Generator[S]:
        def generate = f(g.generate)
    def flatMap(f: T => Generator[S]) = new Generator[S]:
        def generate = f(g.generate).generate

val integers = new Generator[Int] :
    def generate: Int = java.util.Random().nextInt

val booleans = for x <- integers yield x > 0

def pairs[T, U](t : Generator[T], u : Generator[U]) = for x <- t ; y <- u yield (x, y)

def single[T](t : T) = new Generator[T] :
    def generate: T = t

def range(from : Int, to : Int) = for x <- integers yield from + x.abs % (to - from)

def OneOf[T](bindings : T*) = 
    for ind <- range(0, bindings.length) yield bindings(ind)

def lists[T](t: Generator[T]) : Generator[List[T]] = 
    for
        x <- range(0, 6)
        list <- if x == 0 then emptyLists else nonEmptyLists(t)
    yield list

val emptyLists = single(Nil)

def nonEmptyLists[T] (t : Generator[T]) = 
    for
        x <- t
        list <- lists(t)
    yield x :: list


enum Tree[T]:
    case Leaf(value : T)
    case Inner(left : Tree[T], right : Tree[T])

def trees[T](g : Generator[T]) : Generator[Tree[T]] =
    for
        x <- booleans
        tree <- if x  then leafTrees(g) else innerTrees(g)
    yield tree 

def leafTrees[T](g: Generator[T]) = single(Tree.Leaf(g.generate))

def innerTrees[T](g: Generator[T]) =
    for 
        left <- trees(g)
        right <- trees(g)
    yield Tree.Inner(left, right)

@main def test = 
        
    (1 to 10).foreach(_ => println(pairs(integers, booleans).generate))
    (1 to 10).foreach(_ => println(single("singleton").generate))
    (1 to 10).foreach(_ => println(range(1, 1000).generate))
    (1 to 10).foreach(_ => println(OneOf(1, 5, 8, 9).generate))
    (1 to 10).foreach(_ => println(lists(OneOf(1, 5, 8, 9)).generate))
    (1 to 10).foreach(_ => println(trees(OneOf(1, 5, 8, 9)).generate))
    

