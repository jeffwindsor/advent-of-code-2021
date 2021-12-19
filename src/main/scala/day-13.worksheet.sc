import scala.annotation.tailrec

trait Fold
case class FoldX(at:Int) extends Fold
case class FoldY(at:Int) extends Fold
type Points = Set[Point]

def isDataPoint(s:String) = !s.startsWith("fold")
def dataPoints(filename:String) =
  Input.asNonEmptyLines(filename).takeWhile(isDataPoint).map(_.split(","))
    .map(a => Point(a(0).toInt,a(1).toInt)).toSet
def dataFolds(filename:String) =
  Input.asNonEmptyLines(filename).dropWhile(isDataPoint).map(_.split("=")).map {
      case Array("fold along y", y) => FoldY(y.toInt)
      case Array("fold along x", x) => FoldX(x.toInt) }

def foldPaper(ps:Points, fp:Fold):Points = fp match
  case FoldX(at) => ps.filter(at > _.x) ++ ps.filter(at < _.x).map(p => Point(2 * at - p.x, p.y))
  case FoldY(at) => ps.filter(at > _.y) ++ ps.filter(at < _.y).map(p => Point(p.x, 2 * at - p.y))

@tailrec
def foldPapers(ps:Points, folds:List[Fold]): Points = folds match
  case f::fs  => foldPapers(foldPaper(ps,f), fs)
  case List() => ps

def part1(f:String) = foldPaper(dataPoints(f), dataFolds(f).head).size
def part2(f:String) = foldPapers(dataPoints(f),dataFolds(f))

Output.printResults(13,part1,part2)

//================================================================================
def printPaper(ps:Points): Unit = {
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
