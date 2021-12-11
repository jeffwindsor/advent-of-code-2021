import scala.collection.mutable.{HashSet, Queue}

case class Point(r:Int,c:Int)
type Energy = List[Array[Int]]
case class Grid(rows:Int,cols:Int,e:Energy, ps:List[Point])

def energyAtPoint(g:Grid)(p:Point) = g.e(p.r)(p.c)
def incrementPoint(g:Grid)(p:Point) = g.e(p.r)(p.c) += 1
def incrementGrid(g:Grid) = for(p <- g.ps) incrementPoint(g)(p)
def resetPoint(g:Grid)(p:Point) = g.e(p.r)(p.c) = 0
def validPoint(g:Grid)(p:Point) = p.r >= 0 && p.r < g.rows && p.c >= 0 && p.c < g.cols

def adjacentPoints(p:Point) = (for(r <- p.r-1 to p.r+1; c <- p.c-1 to p.c+1) yield Point(r,c)).filter(_ != p)
def effectedPoints(p:Point)= adjacentPoints(p).filter(validPoint(g)(_)).filter(energyAtPoint(g)(_) <= 9)

def flashGrid(g:Grid) = {
  val flashed = new Queue[Point]
  for(p <- g.ps.filter(energyAtPoint(g)(_) > 9)){
    val fs = flashBasin(g)(p)
    flashed ++= fs 
  }
  for(p <- flashed) resetPoint(g)(p)
}
def flashBasin(g:Grid)(flashPoint:Point):Iterable[Point] = {
  val todo    = new Queue[Point].addOne(flashPoint)
  val flashed = new Queue[Point]

  while(todo.isEmpty == false){
    val p = todo.dequeue
    if(energyAtPoint(g)(p) > 9){
      flashed += p

      val incs = effectedPoints(p)
      for(i <- incs) incrementPoint(g)(i)
      todo ++= incs.filter(energyAtPoint(g)(_) > 9)  
    }
  }
  flashed.toIterable
}
//=============================================================================
def readFrom(filename:String):Grid = {
  val e = scala.io.Source.fromFile(filename).getLines
    .filter(_.nonEmpty)
    .map(_.map(_.asDigit).toArray)
    .toList

    val rows = e.length
    val cols = e(0).length
    Grid(rows,cols, e, (0 until rows).flatMap(r => (0 until cols).map(Point(r,_))).toList) 
}
//=============================================================================
def printEnergy(g:Grid){    
  for(r <- g.e){ 
    for(c <- r) if(c < 1 || c > 9) print(Console.BLUE + c + Console.WHITE + ",") else print(c + ",")
      println 
    }
}
val g = readFrom("data/day-11-example.txt")
printEnergy(g)
for(i <- 1 to 2){
  println("--" + i + "------------------------------")
  incrementGrid(g)
  flashGrid(g)
  printEnergy(g)
}
