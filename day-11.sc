import scala.collection.mutable.{HashSet, Queue}

case class Point(r:Int,c:Int)
case class Grid(rows:Int,cols:Int,e:List[Array[Int]], ps:List[Point]){
  def energy(p:Point) = this.e(p.r)(p.c)
  def increment(p:Point) = this.e(p.r)(p.c) += 1
  def reset(p:Point) = this.e(p.r)(p.c) = 0
  def isvalid(p:Point) = p.r >= 0 && p.r < this.rows && p.c >= 0 && p.c < this.cols
  def flash = {
    val flashed = new Queue[Point]
    for(p <- this.ps) this.increment(p)
    for(p <- this.ps.filter(this.energy(_) > 9)){
      val fs = this.flashBasin(p)
      flashed ++= fs 
    }
    for(p <- flashed) this.reset(p)
    flashed.length
  }
  private def flashBasin(flashPoint:Point):Iterable[Point] = {
    val todo    = new Queue[Point].addOne(flashPoint)
    val flashed = new Queue[Point]

    while(todo.isEmpty == false){
      val p = todo.dequeue
      if(this.energy(p) > 9){
        flashed += p

        val incs= (for(r <- p.r-1 to p.r+1; c <- p.c-1 to p.c+1) yield Point(r,c))
          .filter(_ != p)
          .filter(this.isvalid(_))
          .filter(this.energy(_) <= 9)

        for(i <- incs) this.increment(i)
        todo ++= incs.filter(this.energy(_) > 9)  
      }
    }
    flashed.toIterable
  }
  def printEnergyGrid = {
    for(r <- this.e){ 
      for(c <- r) if(c < 1 || c > 9) print(Console.BLUE + c + Console.WHITE + ",") else print(c + ",")
        println 
      }
  } 
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
def part1(g:Grid) = (1 to 100).foldLeft(0)((total,_) => total + g.flash)
def part2(g:Grid) = {
  var count = 1
  var all   = g.rows * g.cols
  while(g.flash != all) count += 1
  count
}

println(part1(readFrom("data/day-11-example.txt")))
println(part1(readFrom("data/day-11.txt")))
println(part2(readFrom("data/day-11-example.txt")))
println(part2(readFrom("data/day-11.txt")))

