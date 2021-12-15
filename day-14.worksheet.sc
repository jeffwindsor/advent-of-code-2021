//=============================================================================
// Advent of Code 2021 --- Day 14: Extended Polymerization ---
//=============================================================================
// Principle:
type Pair = String
type PairCount = (Pair,BigInt)
type PairCounts = Map[Pair,BigInt]
type PairInserts = Map[Pair, String]
case class Input(pcs:PairCounts, pis:PairInserts)

def parse(filename:String):Input = {
  val inputs = scala.io.Source.fromFile(filename).getLines.span(!_.isEmpty)
  val template = inputs._1.toList.head.sliding(2).toList.groupBy(identity).map{ case(k,vs) => (k, BigInt(vs.length))}
  val inserts  = inputs._2.filter(!_.isEmpty).map(_.split(" -> ")).map(a => a(0) -> a(1)).toMap
  Input(template,inserts) }

def addInsert(pc:PairCount, pis:PairInserts) = {
  val (pair, count) = pc
  pis.get(pair) match {
    case Some(insert) => List((pair(0).toString + insert, count), (insert + pair(1).toString, count))
    case None         => List(pc) }}

def addInserts(pcs:PairCounts, pis:PairInserts) = pcs
  .map(pc => addInsert(pc, pis)).flatten
  .groupMapReduce(_._1)(_._2)(_ + _)

def answer(steps:Int, pcs:PairCounts, pis:PairInserts) = {
  val finalPairCounts = (0 until steps).foldLeft(pcs)((acc,_) => addInserts(acc, pis))
  val letterCounts = finalPairCounts.toList
    .map{ case (p,c) => (p(1),c) }
    .groupMapReduce(_._1)(_._2)(_ + _)
  val counts = letterCounts.map(_._2)
  counts.max - counts.min }

def part1(i:Input) = answer(10,i.pcs,i.pis)
def part2(i:Input) = answer(40,i.pcs,i.pis)

//==ANSWERS====================================================================
println("Advent of Code 2021 --- Day 14: Extended Polymerization ---")
println(" part 1 : example : " + part1(parse("data/14e")))
println(" part 1 : actual : " + part1(parse("data/14")))
println(" part 2 : example : " + part2(parse("data/14e")))
println(" part 2 : actual : " + part2(parse("data/14")))
