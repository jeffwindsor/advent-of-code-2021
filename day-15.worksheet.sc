//=============================================================================
// Advent of Code 2021 --- Day 15: Chiton ---
//=============================================================================
// Principle:
import annotation.tailrec
import scala.collection.mutable.{PriorityQueue,HashMap}

case class Point(r:Int,c:Int){ def +(that:Point) = Point(this.r+that.r, this.c+that.c) }
case class QueueItem(total:Int,point:Point)
type RiskMap = HashMap[Point, Int]
type MapPoint = (Point, Int)

def parse(filename:String) = {
  val inputs= scala.io.Source.fromFile(filename).getLines.zipWithIndex.map { 
      case (l,r) => l.zipWithIndex.map { 
        case (risk,c) => Point(r,c) -> risk.asDigit }}.flatten
  new HashMap() ++ inputs
}

val byLowestTotalRisk = Ordering.by((r:QueueItem) => r.total).reverse
val start = QueueItem(0, Point(0,0))
def startQueue = new PriorityQueue[QueueItem]()(byLowestTotalRisk) += start
def neighbors = List(Point(0,1),Point(0,-1),Point(1,0),Point(-1,0))
def validPoint(p:Point, limit:Int) = p.r >= 0 && p.r <= limit && p.c >= 0 && p.c <= limit
def maxIndex(rm:RiskMap) = rm.keys.map(_.r).max

@tailrec
def shortestPath(riskMap:RiskMap, pq:PriorityQueue[QueueItem], limit:Int): Int = {
  val current = pq.dequeue
  if(current.point.r == limit && current.point.c == limit) { return current.total }
  
  riskMap -= current.point
  val adds  = neighbors.map(current.point + _)
      .filter(validPoint(_,limit))
      .filter(riskMap.contains(_))
      .map(p => QueueItem(current.total + riskMap(p), p))
  for(a <- adds) {
    pq.enqueue(a)
    riskMap -= a.point
  }
  shortestPath(riskMap, pq, limit)
}
def part1(rm:RiskMap) = shortestPath(rm, startQueue, maxIndex(rm))

def shiftRisk(risk:Int, expandBy:Int) = (risk + expandBy - 1) % 9 + 1
def shiftRow(gridSize:Int)(row:Int) = Point(gridSize * row, 0)
def shiftCol(gridSize:Int)(col:Int) = Point(0, gridSize * col)
def shiftGrids(shift:Int => Point)(mp:MapPoint, times:Int, size:Int) = 
  (1 until times).map(i => (mp._1 + shift(i), shiftRisk(mp._2, i)))
def expand(riskMap:RiskMap, times:Int) = {
  val size = maxIndex(riskMap) + 1
  for(mp <- riskMap.toList) riskMap ++= shiftGrids(shiftRow(size))(mp, times, size)
  for(mp <- riskMap.toList) riskMap ++= shiftGrids(shiftCol(size))(mp, times, size)
}
def part2(rm:RiskMap) = { 
  expand(rm,5)
  shortestPath(rm, startQueue, maxIndex(rm))
}

//==ANSWERS====================================================================
println("Advent of Code 2021 --- Day 15: Chiton ---")
println(" part 1 : example : " + part1(parse("data/15e")))
println(" part 1 : actual  : " + part1(parse("data/15")))
println(" part 2 : example : " + part2(parse("data/15e")))
println(" part 2 : actual  : " + part2(parse("data/15")))