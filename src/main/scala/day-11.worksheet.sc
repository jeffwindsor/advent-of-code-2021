import shared.*
import scala.collection.mutable

case class Grid(rows:Int,cols:Int,e:List[Array[Int]], ps:List[Point])

def data(filename:String):Grid = 
  val e = Input.asNonEmptyLines(filename).map(_.map(_.asDigit).toArray)
  val rows = e.length
  val cols = e.head.length
  Grid(rows,cols, e, (0 until rows).flatMap(r => (0 until cols).map(Point(_,r))).toList) 

def energy(g:Grid, p:Point):Int = g.e(p.y)(p.x)
def increment(g:Grid, p:Point): Unit = g.e(p.y)(p.x) += 1
def reset(g:Grid, p:Point): Unit = g.e(p.y)(p.x) = 0
def isValid(g:Grid, p:Point):Boolean = p.y >= 0 && p.y < g.rows && p.x >= 0 && p.x < g.cols

def flashBasin(g:Grid, flashPoint:Point):Iterable[Point] = 
  val todo    = new mutable.Queue[Point].addOne(flashPoint)
  val flashed = new mutable.Queue[Point]

  while(todo.nonEmpty)
    val p = todo.dequeue
    if(energy(g, p) > 9)
      flashed += p
      val incs = //Point.range(p.x-1, p.x+1,p.y-1,p.y+1) 
        (for(r <- p.y-1 to p.y+1; c <- p.x-1 to p.x+1) yield Point(c,r))
        .filter(_ != p)
        .filter(isValid(g,_))
        .filter(energy(g, _) <= 9)
      for(i <- incs) increment(g, i)
      todo ++= incs.filter(energy(g, _) > 9)
  flashed

def flash(g:Grid) = 
  val flashed = new mutable.Queue[Point]
  for(p <- g.ps) increment(g,p)
  for(p <- g.ps.filter(energy(g,_) > 9)) flashed ++= flashBasin(g,p)
  for(p <- flashed) reset(g,p)
  flashed.length

def part1(f:String) = (1 to 100).foldLeft(0)((total,_) => total + flash(data(f)))
def part2(f:String) = {
  val g = data(f)
  var count = 1
  val all   = g.rows * g.cols
  while(flash(g) != all) count += 1
  count
}

Output.printResults(11,part1)
Output.printResults(11,part2)

def printEnergyGrid(g:Grid): Unit = 
  for(r <- g.e)
    for(c <- r) 
      if(c < 1 || c > 9) print(Console.BLUE + c + Console.WHITE + ",") 
      else print(s"$c,")
    println()