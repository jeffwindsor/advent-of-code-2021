import scala.annotation.tailrec

type Graph = Map[String, List[String]]
type Node = String
val separator = ","

def data(filename:String) =
  val inputs = Input.asNonEmptyLines(filename).map(_.split('-'))
  (inputs.map(a => (a(0),a(1))) ++ inputs.map(a =>(a(1),a(0))))
    .groupMap{ case (a,_) => a }{ case (_,b) => b}

def findPathsVisitSmallCavesOnce(g:Graph, visited:Set[Node], current:Node):Seq[String] = {
  val visitedNow = if(current == current.toUpperCase) visited else visited + current
  g(current).flatMap {
    case "end" => Seq(current + separator + "end")
    case "start" => Seq(current)
    case v if visitedNow.contains(v) => Seq(current)
    case v => findPathsVisitSmallCavesOnce(g, visitedNow, v).map(current + separator + _)
  }
}

def findPathsVisitSmallCavesSpecial(g:Graph) = {
  def inner(visited: Set[Node])(current: Node, special: Option[Node]): Seq[String] = {
    val visitedNow = if (current == current.toUpperCase) visited else visited + current
    val innerCurry = inner(visitedNow) _
    g(current).flatMap {
      case "end" => Seq(current + separator + "end")
      case "start" => Seq(current)
      case v if visitedNow.contains(v) => special match {
        case Some(_) => Seq(current)
        case None => innerCurry(v, Some(v)).map(current + separator + _)
      }
      case v => innerCurry(v, special).map(current + separator + _)
    }
  }
  inner(Set.empty)("start", None)
}

def part1(f:String) = findPathsVisitSmallCavesOnce(data(f), Set.empty, "start").count(_.endsWith("end"))
def part2(f:String) = findPathsVisitSmallCavesSpecial(data(f)).count(_.endsWith("end"))
Output.printResults(12,part1,part2)