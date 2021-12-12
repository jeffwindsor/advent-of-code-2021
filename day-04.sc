import scala.annotation.tailrec

val boardsize = 5
case class Game(numbers:List[Int], boards: List[Board]) {
  private val indexMap = numbers.zipWithIndex.toMap
  @tailrec
  private def playNumbers(numbers:List[Int], boards: List[Board]):List[Board] = numbers match {
    case List() => boards
    case _      => playNumbers(numbers.tail, boards.map(_.play(numbers.head)))}
  def play:List[Result] = playNumbers(numbers, boards)
    .filter{ case _:Winner => true }
    .map{ case Winner(closingPlay,score) => Result(indexMap(closingPlay), score) }
    .toList
}

trait Board {
  def play(number:Int): Board = this match {
    case w:Winner => w
    case Playable(cells,rowMatches,colMatches) => {
      // imperative / mutable approach: set cell to matched, and inc the row and col count of matches
      for(row <- 0 until boardsize; col <- 0 until boardsize){
        if(cells(row)(col).number == number){
          cells(row)(col) = Cell(number,true)
          rowMatches(row) += 1
          colMatches(col) += 1 }
      }
      // test / score outcome
      if(rowMatches.contains(boardsize) || colMatches.contains(boardsize)) {
        val sumOfNonMatches = cells.flatten.filter(!_.matched).map(_.number).sum
        Winner(number, number * sumOfNonMatches)
      } else Playable(cells, rowMatches, colMatches)
    }
  }
}
case class Winner(closingPlay: Int, score: Int) extends Board
case class Playable(cells: List[Array[Cell]], rowMatches:Array[Int], colMatches:Array[Int]) extends Board 

case class Cell(number:Int, matched: Boolean = false)
case class Result(index:Int, score:Int)

//========================================================================================
def read(filename:String) = {
    def boardfromFile(boardInput:Iterable[String]) = {
      val cells = boardInput
        .map(rowInput => rowInput.trim.split("\\D+")
        .map(cellInput => Cell(cellInput.toInt)).toArray).toList
      Playable(cells, new Array[Int](boardsize), new Array[Int](boardsize)) }
  val input   = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  val numbers = input.next.split(",").filter(_.nonEmpty).map(_.toInt).toList
  val boards  = input.sliding(boardsize, boardsize).map(boardfromFile(_)).toList
  Game(numbers, boards)
}
//========================================================================================
def playToWin(game: Game):Int  = game.play.sortBy(_.index).head.score
def playToLose(game: Game):Int = game.play.sortBy(_.index)(Ordering[Int].reverse).head.score

for(file <- List("data/day-4-example.txt", "data/day-4.txt"); 
    play <- List(playToWin(_), playToLose(_))) { 
    println(file + ": " + play(read(file)))
}
