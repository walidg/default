import scala.math._

enum Move:
        case Empty(ind : Int)
        case Fill(ind : Int)
        case Pour(i1 : Int, i2 : Int)

case class State(xs : Int*):
    def updated(ind: Int, value: Int) : State =
        State(xs.updated(ind, value)*)

type Path = List[State]

class Problem(full : State):

    val dimension = full.xs.length
    lazy val start = List(State(List.fill(dimension){0}*))
   
    def move(state: State, m: Move) : State = m match
        case Move.Empty(i) => state.updated(i, 0)
        case Move.Fill(i) => state.updated(i, full.xs(i))
        case Move.Pour(i1, i2) => 
            val pouredAmount = min(state.xs(i1), (full.xs(i2) - state.xs(i2)))
            state.updated(i1, state.xs(i1) - pouredAmount).updated(i2, state.xs(i2) + pouredAmount)

    def extend(paths : List[Path], explored : Set[State]) : LazyList[List[Path]] =
        def isExplored(s: State) = explored.contains(s)
        val extension = (for 
            p <- paths
            m <- allMoves 
            if !(isExplored(move(p.head, m))) 
        yield move(p.head, m) :: p)
        paths #:: extend(extension, explored ++ extension.flatten)
    
    lazy val allMoves : List[Move] = 
        (for i <- 0 until dimension yield Move.Empty(i)).toList ++
        (for i <- 0 until dimension yield Move.Fill(i)) ++
        (for i <- 0 until dimension ; j <- 0 until dimension if i != j yield Move.Pour(i, j))

    def solve(target : Int) : LazyList[Path] = 
        for
            paths <- extend(List(start), Set())
            path <- paths
            if path.head.xs.contains(target)
        yield path

@main def test =
    val problem = Problem(full = State(7, 4, 5))
    println(problem.solve(6).head.reverse)
    assert(problem.move(State(0, 0), Move.Fill(0)).xs == List(7, 0))
    assert(problem.move(State(0, 0), Move.Fill(1)).xs == List(0, 4))
    assert(problem.move(State(2, 1), Move.Pour(0, 1)).xs == List(0, 3))
    assert(problem.move(State(6, 1), Move.Pour(0, 1)).xs == List(3, 4))
    assert(problem.move(State(7, 2), Move.Pour(1, 0)).xs == List(7, 2))
    assert(problem.move(State(5, 3), Move.Empty(0)).xs == List(0, 3))
    assert(problem.move(State(4, 1), Move.Empty(1)).xs == List(4, 0))

