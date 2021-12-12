//=============================================================================
// Advent of Code 2021 Day 12 (Passage Pathing)
//=============================================================================
// Principle: 
//
def parse(filename:String) = {
  val inputs = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
    .map(_.split('-')).toList

  val labels    = inputs.flatten.toSet
  val size      = labels.size
  val vertices  = labels.zipWithIndex.toMap
  val undirectedEdges = (inputs.map(a => (a(0),a(1))) ++ inputs.map(a =>(a(1),a(0))))
  val edgeMap   = undirectedEdges.groupMap{ case (a,b) => a }{ case (a,b) => b}
  (size, vertices, edgeMap)
}



//==ANSWERS====================================================================
println("Advent of Code 2021 12 (Passage Pathing)")
println(" part 1 : example : " + parse("data/12e"))
//println(" part 1 : actual  : " + powerConsumption(parse("data/12")))
//println(" part 2 : example : " + lifeSupport(parse("data/12e")))
//println(" part 2 : actual  : " + lifeSupport(parse("data/12")))
