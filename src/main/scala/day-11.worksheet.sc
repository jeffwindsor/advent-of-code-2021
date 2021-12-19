import scala.collection.mutable

case class Point(r:Int,c:Int)
case class Grid(rows:Int,cols:Int,e:List[Array[Int]], ps:List[Point])

def data(filename:String):Grid = {
  val e = Input.asNonEmptyLines(filename).map(_.map(_.asDigit).toArray)
  val rows = e.length
  val cols = e.head.length
  Grid(rows,cols, e, (0 until rows).flatMap(r => (0 until cols).map(Point(r,_))).toList) }


def energy(g:Grid, p:Point):Int = g.e(p.r)(p.c)
def increment(g:Grid, p:Point): Unit = g.e(p.r)(p.c) += 1
def reset(g:Grid, p:Point): Unit = g.e(p.r)(p.c) = 0
def isValid(g:Grid, p:Point):Boolean = p.r >= 0 && p.r < g.rows && p.c >= 0 && p.c < g.cols

def flashBasin(g:Grid, flashPoint:Point):Iterable[Point] = {
  val todo    = new mutable.Queue[Point].addOne(flashPoint)
  val flashed = new mutable.Queue[Point]

  while(todo.nonEmpty){
    val p = todo.dequeue
    if(energy(g, p) > 9){
      flashed += p
      val incs= (for(r <- p.r-1 to p.r+1; c <- p.c-1 to p.c+1) yield Point(r,c))
        .filter(_ != p)
        .filter(isValid(g,_))
        .filter(energy(g, _) <= 9)
      for(i <- incs) increment(g, i)
      todo ++= incs.filter(energy(g, _) > 9) }}
  flashed
}

def flash(g:Grid) = {
  val flashed = new mutable.Queue[Point]
  for(p <- g.ps) increment(g,p)
  for(p <- g.ps.filter(energy(g,_) > 9)) flashed ++= flashBasin(g,p)
  for(p <- flashed) reset(g,p)
  flashed.length
}

def printEnergyGrid(g:Grid): Unit = {
  for(r <- g.e){
    for(c <- r) if(c < 1 || c > 9) print(Console.BLUE + c + Console.WHITE + ",") else print(c + ",")
      println }}

def part1(filename:String) = {
  val g = data(filename)
  (1 to 100).foldLeft(0)((total,_) => total + flash(g))
}
def part2(filename:String) = {
  val g = data(filename)
  var count = 1
  val all   = g.rows * g.cols
  while(flash(g) != all) count += 1
  count
}

Output.printResults(11,part1,part2)