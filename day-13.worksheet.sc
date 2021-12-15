
//=============================================================================
// Advent of Code 2021 --- Day 13: Transparent Origami ---
//=============================================================================
// Principle: graphs, depth first search 
//
trait FoldPaper
case class FoldX(at:Int) extends FoldPaper
case class FoldY(at:Int) extends FoldPaper
case class Point(x:Int,y:Int)
type Points = Set[Point]
case class Input(points:Points, folds:List[FoldPaper])

def parse(filename:String) = {
  val inputs = scala.io.Source.fromFile(filename).getLines.span(!_.isEmpty)
  val points = inputs._1.map(_.split(",")).map(a => Point(a(0).toInt,a(1).toInt)).toSet
  val folds  = inputs._2.drop(1).map(_.split("=")).map{
    case Array("fold along y", y) => FoldY(y.toInt)
    case Array("fold along x", x) => FoldX(x.toInt) }
    .toList
  Input(points,folds)
}

def foldPaper(ps:Points, fp:FoldPaper):Points = fp match {
  case FoldX(at) => ps.filter(at > _.x) ++ ps.filter(at < _.x).map(p => Point(2 * at - p.x, p.y))
  case FoldY(at) => ps.filter(at > _.y) ++ ps.filter(at < _.y).map(p => Point(p.x, 2 * at - p.y))
}

def part1(i:Input) = foldPaper(i.points, i.folds.head).size
def part2(i:Input):Points = i.folds match {
    case f::fs  => part2(Input(foldPaper(i.points, f),fs))
    case List() => i.points
}


//==ANSWERS====================================================================
println("Advent of Code 2021 --- Day 13: Transparent Origami ---")
println(" part 1 : example : " + part1(parse("data/13e")))
println(" part 1 : actual  : " + part1(parse("data/13")))
println("part 2 : example : ")
printPaper(part2(parse("data/13e")))
println("part 2 : actual  : ")
printPaper(part2(parse("data/13")))

def printPaper(ps:Points) = {
  val mx = ps.map(_.x).max
  val my = ps.map(_.y).max
  val g = ps.groupMap(_.y)(_.x)

  for(y <- 0 to my){
    val row = g.get(y) match {
      case Some(gps) => (0 to mx).map(x => if(gps.contains(x)) '#' else '.').mkString
      case None => (0 to mx).map(_ => '.').mkString
    }
    println(row)
  }
  println
}
