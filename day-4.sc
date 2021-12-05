val boardsize = 5
val indexes = 0 until boardsize
val noWin = -1

type Answer = Int
type Number = Int
type MatchCounts = Array[Int]

case class Cell(number:Number, matched: Boolean)
class Board(cells: List[Array[Cell]]){
  private val rowMatchCounts:MatchCounts = new Array[Int](boardsize)
  private val colMatchCounts:MatchCounts = new Array[Int](boardsize)
  var winningPlay:Number = noWin
  var score:Number = 0

  private def sumOfNonMatches = cells.flatten.filter(!_.matched).map(_.number).sum
  def play(number:Number):Unit = 
    if( winningPlay == noWin) {
      // Not a winner yet, apply number
      for(row <- indexes; col <- indexes){
        if(cells(row)(col).number == number){
          // set cell to matched, and inc the row and col count of matches
          cells(row)(col) = Cell(number,true)
          rowMatchCounts(row) += 1
          colMatchCounts(col) += 1
      }}
      if(rowMatchCounts.contains(boardsize) || colMatchCounts.contains(boardsize)) {
        winningPlay = number
        score = number * sumOfNonMatches
      }
    } 
}

case class Game(numbers:List[Number], boards: List[Board]){
  def winners:List[Board] = boards.filter(_.winningPlay != noWin)
  def losers:List[Board]  = boards.filter(_.winningPlay == noWin) 
  def play(number:Number):Unit = for(board <- boards) board.play(number)
}
object Game{
  def readFrom(filename:String) = {
    val input        = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
    val numbers      = input.next.split(",").filter(_.nonEmpty).map(_.toInt).toList
    val boards       = input.sliding(boardsize, boardsize).map(
      boardInput => new Board(boardInput.map( 
        rowInput => rowInput.trim.split("\\D+").map(
          cellInput => Cell(cellInput.toInt, false)
        ).toArray
      ).toList)
    ).toList
    Game(numbers, boards)
  }
}
//========================================================================================
def playToWin(game: Game):Answer = {
  for(number <- game.numbers){
    game.play(number)
    val winners = game.winners
    if(!winners.isEmpty) return winners.head.score
  }
  return -1
}

def playToLose(game: Game):Answer = {
  for(number <- game.numbers) game.play(number)
  val indexMap = game.numbers.zipWithIndex.toMap
  val boardsInWinOrderDesc = game.boards
    .filter(_.winningPlay != noWin)
    .map( b => (indexMap(b.winningPlay), b))
    .sortWith(_._1 > _._1)
  boardsInWinOrderDesc.head._2.score
}

for(file <- List("data/day-4-example.txt", "data/day-4.txt"); 
    function <- List(playToWin(_), playToLose(_))
    ) { println(file + ": " + function( Game.readFrom(file))) }
