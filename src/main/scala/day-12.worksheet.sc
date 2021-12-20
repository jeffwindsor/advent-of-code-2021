import scala.annotation.tailrec
import shared.{Input, Output}

type Graph = Map[String, List[String]]
type Node = String
val separator = ","

def data(filename:String) =
  val inputs = Input.asNonEmptyLines(filename).map(_.split('-'))
  (inputs.map(a => (a(0),a(1))) ++ inputs.map(a =>(a(1),a(0))))
    .groupMap{ case (a,_) => a }{ case (_,b) => b}

def findPathsVisitSmallCaves(g:Graph, visited:Set[Node], current:Node, visitTwice: Option[Node]):Seq[String] = {
  val vistedOnce = if(current == current.toUpperCase) visited else visited + current
  g(current).flatMap {
    case "end" => Seq(current + separator + "end")
    case "start" => Seq(current)
    case v if vistedOnce.contains(v) => visitTwice match {
      case Some(_) => Seq(current)
      case None =>  findPathsVisitSmallCaves(g, vistedOnce, v, Some(v)).map(current + separator + _)
    }
    case v => findPathsVisitSmallCaves(g, vistedOnce, v, visitTwice).map(current + separator + _)
  }
}

def part1(f:String) = findPathsVisitSmallCaves(data(f), Set.empty, "start", Some("none-twice")).count(_.endsWith("end"))
def part2(f:String) = findPathsVisitSmallCaves(data(f), Set.empty, "start", None).count(_.endsWith("end"))

Output.printResults(12, part1, part2)