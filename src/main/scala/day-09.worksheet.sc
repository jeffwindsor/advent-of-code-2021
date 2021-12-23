import shared.{Input, Output,Point}
import scala.collection.mutable

val wall = 9
type Height = Int
type Heights = List[Array[Int]]

def data(filename:String) = Input.asNonEmptyLines(filename).map(_.map(_.asDigit).toArray)
def points(hs:Heights) = hs.indices.flatMap(r => hs(r).indices.map(Point(r,_)))
def height(hs:Heights)(p:Point):Height = hs(p.x)(p.y)
def risk(hs:Heights)(p:Point):Int = height(hs)(p) + 1
def lowpoint(hs:Heights)(p:Point):Boolean = height(hs)(p) match {
  case 9 => false
  case h => h < p.neighbors.map(height(hs)(_)).min }

def lowpoints(hs:Heights) =
  points(hs).filter(lowpoint(hs)(_))

def basinpoints(hs:Heights)(lowpoint:Point) = {
  val todo = new mutable.Queue[Point].addOne(lowpoint)
  val seen = new mutable.Queue[Point]
  val h = height(hs) _

  while(todo.nonEmpty){
    val p = todo.dequeue
    seen += p
    todo ++= p.neighbors.filter(h(_) != 9).diff(seen).diff(todo)
  }
  seen
}

def part1(f:String) = {
  val hs = data(f)
  lowpoints(hs).map(risk(hs)(_)).sum
}
def part2(f:String) = {
  val hs = data(f)
  lowpoints(hs).map(basinpoints(hs)(_).size)
    .sorted(Ordering.Int.reverse).take(3).product
}

Output.printResults(9,part1)
Output.printResults(9,part2)
