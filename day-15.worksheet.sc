//=============================================================================
// Advent of Code 2021 --- Day 15: Chiton ---
//=============================================================================
// Principle:
import annotation.tailrec
import scala.collection.mutable.{PriorityQueue,HashMap}

case class Point(r:Int,c:Int){ def +(that:Point) = Point(this.r+that.r, this.c+that.c) }
case class QueueItem(total:Int,point:Point)
type RiskMap = HashMap[Point, Int]

def parse(filename:String) = {
  val maps = scala.io.Source.fromFile(filename).getLines
    .zipWithIndex.map { 
      case (line,row) => line.zipWithIndex.map { 
        case (risk,col) => Point(row,col) -> risk.asDigit } 
    }.flatten
  new HashMap() ++ maps
}
def expandRisk(risk:Int, expandBy:Int) = {
  val result = (risk + expandBy) % 9
  if(result == 0) 9 else result
}
def expand(riskMap:RiskMap, expansions:Int) = {
  val es = (1 until expansions)
  val m = riskMap.keys.map(_.r).max + 1
  for(rm <- riskMap.toList) riskMap ++= es.map(i => rm._1 + Point((m*i), 0) -> expandRisk(rm._2, i))
  for(rm <- riskMap.toList) riskMap ++= es.map(i => rm._1 + Point(0, (m*i)) -> expandRisk(rm._2, i))
}

val byLowestTotalRisk = Ordering.by((r:QueueItem) => r.total).reverse
def startQueue = new PriorityQueue[QueueItem]()(byLowestTotalRisk) += QueueItem(0, Point(0,0))
def neighbors = List(Point(0,1),Point(0,-1),Point(1,0),Point(-1,0))
def validPoint(p:Point, limit:Int) = p.r >= 0 && p.r <= limit && p.c >= 0 && p.c <= limit
def size(rm:RiskMap) = rm.keys.map(_.r).max

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

def part1(rm:RiskMap) = shortestPath(rm, startQueue, rm.keys.map(_.r).max)
def part2(rm:RiskMap) = { 
  expand(rm,5)
  shortestPath(rm, startQueue, rm.keys.map(_.r).max)
}

//==ANSWERS====================================================================
println("Advent of Code 2021 --- Day 15: Chiton ---")
println(" part 1 : example : " + part1(parse("data/15e")))
println(" part 1 : actual  : " + part1(parse("data/15")))
println(" part 2 : example : " + part2(parse("data/15e")))
println(" part 2 : actual  : " + part2(parse("data/15")))

//val max = test.keys.map(_.r).max
//for(r <- 0 to max) {
//  for(c <- 0 to max) { 
//    print(test(Point(r,c)) + " ") 
//  }
//  println
//}