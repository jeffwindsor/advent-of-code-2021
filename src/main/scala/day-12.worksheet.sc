type Graph = Map[String, List[String]]
type Node = String
val separator = ","

def data(filename:String):Graph = {
  val inputs = Input.asNonEmptyLines(filename).map(_.split('-')).toList
  (inputs.map(a => (a(0),a(1))) ++ inputs.map(a =>(a(1),a(0))))
    .groupMap{ case (a,_) => a }{ case (_,b) => b} }

def findPathsVisitSmallCavesOnce(g:Graph, visited:Set[Node], vertice:Node):Seq[String] = {
  val visitedNow = if(vertice == vertice.toUpperCase) visited else visited + vertice
  g(vertice).flatMap {
    case "end" => Seq(vertice + separator + "end")
    case "start" => Seq(vertice)
    case v if (visitedNow.contains(v)) => Seq(vertice)
    case v => findPathsVisitSmallCavesOnce(g, visitedNow, v).map(vertice + separator + _)
  }
}
def findPathsVisitSmallCavesSpecial(g:Graph, visited:Set[Node], vertice:Node, special:Option[Node]):Seq[String] = {
  val visitedNow = if(vertice == vertice.toUpperCase) visited else visited + vertice
  g(vertice).flatMap {
    case "end" => Seq(vertice + separator + "end")
    case "start" => Seq(vertice)
    case v if (visitedNow.contains(v)) => special match {
      case Some(_) => Seq(vertice)
      case None => findPathsVisitSmallCavesSpecial(g, visitedNow, v, Some(v)).map(vertice + separator + _)
    }
    case v => findPathsVisitSmallCavesSpecial(g, visitedNow, v, special).map(vertice + separator + _)
  }
}

def part1(f:String) = findPathsVisitSmallCavesOnce(data(f), Set.empty, "start").count(_.endsWith("end"))
def part2(f:String) = findPathsVisitSmallCavesSpecial(data(f), Set.empty, "start", None).count(_.endsWith("end"))
Output.printResults(12,part1,part2)