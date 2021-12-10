val wall = 9
case class Point(r:Int,c:Int)
type Heights = List[Array[Int]]

def points(hs:Heights) = (0 until hs.length).flatMap(r => (0 until hs(r).length).map(Point(r,_)))
def adjecents(p:Point) = Seq(Point(p.r,p.c-1),Point(p.r+1,p.c),Point(p.r-1,p.c),Point(p.r,p.c+1))
def lowpoint(hs:Heights)(p:Point):Boolean = height(hs)(p) match {
  case 9 => false
  case h => h < adjecents(p).map(height(hs)(_)).min }
def lowpoints(hs:Heights) = points(hs).filter(lowpoint(hs)(_))
def height(hs:Heights)(p:Point) = hs(p.r)(p.c)
def risk(hs:Heights)(p:Point) = height(hs)(p) + 1
def basin(hs:Heights)(lowpoint:Point) = ???

//=============================================================================
def part1(hs:Heights) = lowpoints(hs).map(risk(hs)(_)).sum
def part2(hs:Heights) = ???
//=============================================================================
def readFrom(filename:String) = {
  val ls = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty).toArray
  ls.map(_.map(_.asDigit).toArray).toList }
//=============================================================================
val inputs = Seq("data/day-9-example.txt", "data/day-9.txt")
  .map(readFrom(_))

for(hs <- inputs) println(part1(hs))
