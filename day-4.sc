val boardsize = 5
val noWinningPlay = -1

case class Cell(number:Int, matched: Boolean)

class Board(cells: List[Array[Cell]]){
  private val rowMatches = new Array[Int](boardsize)
  private val colMatches = new Array[Int](boardsize)
  var winningPlay = noWinningPlay
  var score       = 0

  private def sumOfNonMatches = cells.flatten.filter(!_.matched).map(_.number).sum
  def play(number:Int):Unit = 
    if( winningPlay == noWinningPlay) {
      for(row <- 0 until boardsize; col <- 0 until boardsize){
        if(cells(row)(col).number == number){
          // set cell to matched, and inc the row and col count of matches
          cells(row)(col) = Cell(number,true)
          rowMatches(row) += 1
          colMatches(col) += 1
      }}
      if(rowMatches.contains(boardsize) || colMatches.contains(boardsize)) {
        winningPlay = number
        score = number * sumOfNonMatches
      }
    } 
}
object Board {
  def fromFile(boardInput:Iterable[String]) = new Board(boardInput
    .map(rowInput => rowInput.trim.split("\\D+")
      .map(cellInput => Cell(cellInput.toInt, false)).toArray)
    .toList)
}

case class Game(numbers:List[Int], boards: List[Board]){
  def play(number:Int) = for(board <- boards) board.play(number)
  def winners = boards.filter(_.winningPlay != noWinningPlay)
  def losers  = boards.filter(_.winningPlay == noWinningPlay) 
}
object Game{
  def readFrom(filename:String) = {
    val input   = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
    val numbers = input.next.split(",").filter(_.nonEmpty).map(_.toInt).toList
    val boards  = input.sliding(boardsize, boardsize).map(Board.fromFile(_)).toList
    Game(numbers, boards)
  }
}
//========================================================================================
def playToWin(game: Game):Int = {
  for(number <- game.numbers){
    game.play(number)
    val winners = game.winners
    if(!winners.isEmpty) return winners.head.score
  }
  return -1
}

def playToLose(game: Game):Int = {
  for(number <- game.numbers) game.play(number)
  val indexMap = game.numbers.zipWithIndex.toMap
  val boardsInWinOrderDesc = game.boards
    .filter(_.winningPlay != noWinningPlay)
    .map( b => (indexMap(b.winningPlay), b))
    .sortWith(_._1 > _._1)
  boardsInWinOrderDesc.head._2.score
}

for(file <- List("data/day-4-example.txt", "data/day-4.txt"); 
    function <- List(playToWin(_), playToLose(_))
    ) { println(file + ": " + function( Game.readFrom(file))) }
