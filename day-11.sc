type Grid = List[Array[Int]]
case class Point(r:Int,c:Int)

def inc(g:Grid) = for(r <- 0 until g.length; c <- 0 until g(r).length) g(r)(c) += 1
def energy(g:Grid)(p:Point) = g(p.r)(p.c)
def points(g:Grid) = (0 until g.length).flatMap(r => (0 until g(r).length).map(Point(r,_)))
def flashPoints(g:Grid) = points(g).filter(energy(g)(_) == 10)

//=============================================================================
def part1(g:Grid) = { 
  inc(g)
  val flashes = flashPoints(g)

  flashes.toList
}

//=============================================================================
def readFrom(filename:String):Grid = {
  val ls = scala.io.Source.fromFile(filename).getLines
    .filter(_.nonEmpty)
    .map(_.map(_.asDigit).toArray)
    .toList
  ls }
//=============================================================================
val inputs = Seq("data/day-11-example.txt" //, "data/day-11.txt"
  ).map(readFrom(_))
for (input <- inputs) {
    println(part1(input))
}
