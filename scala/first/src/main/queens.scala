import math.abs

def checks(col : Int, delta : Int, queens : List[Int]) : Boolean = queens match
    case Nil => false
    case x :: xs =>
         x == col    // Vertical check
         || (x - col).abs == delta    // Diagonal check with last row
         || checks(col, delta + 1, xs) 


def isSafe(col: Int, queens: List[Int]) =
    !checks(col, 1, queens)

def queens(n : Int) =
    def placeQueens(k : Int) : Set[List[Int]] =
        if k == 0 then Set(List())
        else 
            for
                queens <- placeQueens(k - 1)
                col <- 0 until n
                if isSafe(col, queens)
            yield col :: queens
    placeQueens(n)

@main def test_queens =
    println(queens(4))
