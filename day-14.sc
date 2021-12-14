//=============================================================================
// Advent of Code 2021 --- Day 14: Extended Polymerization ---
//=============================================================================
// Principle:
type Pair = String
type PairCount = (Pair,Int)
type PairCounts = Map[Pair,Int]
type PairInserts = Map[Pair, String]
case class Input(pcs:PairCounts, pis:PairInserts)


def parse(filename:String):Input = {
  val inputs = scala.io.Source.fromFile(filename).getLines.span(!_.isEmpty)
  val template = inputs._1.toList.head.sliding(2).toList.groupBy(identity).map{ case(k,vs) => (k, vs.length)}
  val inserts  = inputs._2.filter(!_.isEmpty).map(_.split(" -> ")).map(a => a(0) -> a(1)).toMap
  Input(template,inserts)
}
def firstLetter(p:Pair) = p(0)
def lastLetter(p:Pair) = p(1)
def dosomething(pc:PairCount, pis:PairInserts):List[PairCount] = {
  val pair = pc._1
  pis.get(pair) match {
    case Some(insert) => List(pc, (firstLetter(pair) + insert , 1), (insert + lastLetter(pair), 1))
    case None         => List(pc)
  }
}
  
def answer(i:Input) = i.pcs.map(p => dosomething(p, i.pis)).flatten


//==ANSWERS====================================================================
println("Advent of Code 2021 --- Day 14: Extended Polymerization ---")
println(answer(parse("data/14e")))

//println(" part 1 : example : " + part1(e._1, e._2))
//println(" part 2 : example : " + part2(e._1, e._2))

//val a = parse("data/14")
//println(" part 1 : actual  : " + part1(a._1,a._2))
