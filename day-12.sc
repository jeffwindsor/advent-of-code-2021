//=============================================================================
// Advent of Code 2021 Day 12 (Passage Pathing)
//=============================================================================
// Principle: graphs, depth first search 
//
type Graph = Map[String, List[String]]
type Node = String

val start="start"
val end = "end"
val separator = ","

def parse(filename:String):Graph = {
  val inputs = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
    .map(_.split('-')).toList

  val labels    = inputs.flatten.toSet
  val size      = labels.size
  val undirected= (inputs.map(a => (a(0),a(1))) ++ inputs.map(a =>(a(1),a(0))))
  undirected.groupMap{ case (a,b) => a }{ case (a,b) => b}
}

def findPathsVisitSmallCavesOnce(g:Graph, visited:Set[Node], vertice:Node):Seq[String] = {
  val visitedNow = if(vertice == vertice.toUpperCase) visited else visited + vertice
  g(vertice).map(v =>
    if(v == end) Seq(vertice + separator + end)
    else if(v == start) Seq(vertice)
    else if(visitedNow.contains(v)) Seq(vertice)
    else findPathsVisitSmallCavesOnce(g, visitedNow, v).map(vertice + separator + _)
  ).flatten
}
def part1(g:Graph) = findPathsVisitSmallCavesOnce(g, Set.empty, start).filter(_.endsWith(end)).length

def findPathsVisitSmallCavesSpecial(g:Graph, special:Option[Node], visited:Set[Node], vertice:Node):Seq[String] = {
  val visitedNow = if(vertice == vertice.toUpperCase) visited else visited + vertice
  g(vertice).map(v =>
    if(v == end) Seq(vertice + separator + end)
    else if(v == start) Seq(vertice)
    else if(visitedNow.contains(v)){
      if(special == None) findPathsVisitSmallCavesSpecial(g, Some(v), visitedNow, v).map(vertice + separator + _)
      else Seq(vertice) }
    else findPathsVisitSmallCavesSpecial(g, special, visitedNow, v).map(vertice + separator + _)
  ).flatten
}
def part2(g:Graph) = findPathsVisitSmallCavesSpecial(g, None, Set.empty, start).filter(_.endsWith(end)).length


//==ANSWERS====================================================================
println("Advent of Code 2021 12 (Passage Pathing)")
println(" part 1 : example : " + part1(parse("data/12e")))
println(" part 1 : actual  : " + part1(parse("data/12")))
println(" part 2 : example : " + part2(parse("data/12e")))
println(" part 2 : actual  : " + part2(parse("data/12")))
