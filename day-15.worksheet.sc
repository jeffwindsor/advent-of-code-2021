//=============================================================================
// Advent of Code 2021 --- Day 15: Chiton ---
//=============================================================================
// Principle:
import annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

type RiskLevel = Int
case class Cell(r:Int,c:Int)
case class RiskGrid(end:Cell, risks:Map[Cell,RiskLevel])

def parse(filename:String):RiskGrid = {
  val risks = Source.fromFile(filename).getLines
    .zipWithIndex.map { 
      case (line,row) => line.zipWithIndex.map { 
        case (risk,col) => Cell(row,col) -> risk.toInt } 
    }.flatten.toMap
  val rows = risks.keys.map(_.r).max
  val cols = risks.keys.map(_.c).max
  RiskGrid(Cell(rows, cols) ,risks)
}

// @tailrec
def shortestPath(rs:RiskGrid, total:Int, todo:List[Cell]):Int = todo.headOption match {
  case Some(rs.end) | None => total
  case Some(cell)) => {
    val todo1 = todo.tail.appendedAll(neighbors(cell)).sort
    val risk  = rs.risks(cell)
    shortestPath(rs, total + risk, todo.appendedAll().sorted)
}}

val start = Cell(0,0)
def part1(rs:RiskGrid) = shortestPath(rs, 0, List(start))

//==ANSWERS====================================================================
println("Advent of Code 2021 --- Day 15: Chiton ---")
println(" part 1 : example : " + part1(parse("data/15e")))
//println(" part 1 : actual  : " + part1(parse("data/15")))
//println(" part 2 : example : " + part2(parse("data/15e")))
//println(" part 2 : actual  : " + part2(parse("data/15")))
