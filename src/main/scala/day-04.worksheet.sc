import shared.{Input, Output}
import scala.annotation.tailrec

val size = 5

trait Board
case class Winner(closingPlay: Int, score: Int) extends Board
case class Playable(cells: List[Array[Cell]],
                    rowMatches:Array[Int] = new Array[Int](size),
                    colMatches:Array[Int] = new Array[Int](size)) extends Board
case class Game(numbers:List[Int], boards: List[Board])
case class Cell(number:Int, matched: Boolean = false)
case class Result(index:Int, score:Int)

def cells(input:Iterable[String]): List[Array[Cell]] = input
  .map(ri => ri.trim.split("\\D+")
    .map(ci => Cell(ci.toInt))).toList

def data(filename:String): Game = {
  val input   = Input.asNonEmptyLines(filename)
  val numbers = input.head.split(",").map(_.toInt).toList
  val boards  = input.tail.sliding(size, size).map(i => Playable(cells(i))).toList
  Game(numbers, boards) }

@tailrec
private def playNumbers(ns:List[Int], bs: List[Board]): List[Board] = ns match {
  case List() => bs
  case _      => playNumbers(ns.tail, bs.map(play(_, ns.head)))}

def play(g:Game) = {
  val indexMap = g.numbers.zipWithIndex.toMap
  playNumbers(g.numbers, g.boards)
    .filter{ case _:Winner => true }
    .map{ case Winner(closingPlay,score) => Result(indexMap(closingPlay), score) } }

def play(b:Board, number:Int) = b match {
  case w:Winner               => w
  case Playable(cs, rms, cms) => {
    for(row <- 0 until size; col <- 0 until size){
      if(cs(row)(col).number == number){
        cs(row)(col) = Cell(number, matched=true)
        rms(row) += 1
        cms(col) += 1 } }
    if(rms.contains(size) || cms.contains(size)) {
      val sumOfNonMatches = cs.flatten.filter(!_.matched).map(_.number).sum
      Winner(number, number * sumOfNonMatches)
    } else Playable(cs, rms, cms) }}
def part1(f:String) = play(data(f)).minBy(_.index).score
def part2(f:String) = play(data(f)).maxBy(_.index).score

Output.printResults(4,part1)
Output.printResults(4,part2)

