type Value      = Int
type RowId      = Int
type ColId      = Int
type Coordinate = (RowId,ColId)
type Answer     = Int
type Totals     = Map[Int, Int]
type Matches    = Map[Coordinate, Boolean]

case class Board(numbers: Map[Value, Coordinate], matches:Matches,  rowTotals:Totals, colTotals:Totals)
case class Game(numbers:List[Value], boards: List[Board]) 

val boardsize = 5
val indexes = 0 until boardsize

//========================================================================================
def readGame(filename:String) = {
  val emptyMatches = (for(rowId <- indexes; colId <- indexes) yield ((rowId,colId) -> false)).toMap
  val emptyTotals  = (for(id <- indexes) yield (id -> 0)).toMap
  val input        = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  val numbers      = input.next.split(",").filter(_.nonEmpty).map(_.toInt).toList
  val boards       = input.sliding(boardsize, boardsize)
    .map(_.zipWithIndex
      .map{   case (rowString, rowId) => rowString.trim.split("\\D+").zipWithIndex
        .map{ case (cell,colId) => (cell.toInt, (rowId,colId))}})
    .map(ns => Board(ns.flatten.toMap, emptyMatches, emptyTotals, emptyTotals)).toList
  
  Game(numbers, boards)
}
//========================================================================================

def winner(totals:Totals):Boolean = totals.values.toList.contains(boardsize)
def winner(board:Board):Boolean   = winner(board.rowTotals) || winner(board.colTotals)
def winner(boards:List[Board]):Option[Board] = boards match {
  case List() => None
  case boards => if(winner(boards.head)) Some(boards.head) else winner(boards.tail)
}
def score(drawn:Value, board: Board):Answer = {
  val matchedCoordinates = board.matches.filter{ case (k,v) => !v }
  val matchedNumbers     = board.numbers.filter{ case (k,v) => matchedCoordinates.contains(v) }.keys.sum
  drawn * matchedNumbers
} 

def applyNumber(number:Value, board:Board):Board = 
  if(board.numbers.contains(number)){
    val (rowId, colId) = board.numbers(number)
    val ms = (board.matches + ((rowId,colId) -> true))
    val rsi = board.rowTotals(rowId) + 1
    val rs = (board.rowTotals + (rowId -> rsi))
    val csi = board.colTotals(colId) + 1
    val cs = (board.colTotals + (colId -> csi))
    board.copy(matches=ms, rowTotals=rs, colTotals=cs)
  } else board
def applyNumber(number:Value, boards:List[Board]):List[Board] = boards.map(applyNumber(number,_)) 

def play(game: Game):Answer = {
  val drawn = game.numbers.head
  val boards = applyNumber(drawn, game.boards)
  winner(boards) match{
    case None => play(Game(game.numbers.tail, boards))
    case Some(board) => score(drawn, board)
  }
}

///=========================================================================
def printGame(game: Game) = { 
  for(number <- game.numbers) { print(number + " ") }
  println()
    println("--------------------")
  for(board <- game.boards){ 
    println(board) 
    println("--------------------")
  }
}
///=========================================================================

val game = readGame("data/day-4.txt")
printGame(game) 
println( play(game) )

