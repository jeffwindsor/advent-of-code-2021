//=============================================================================
// Advent of Code 2021 --- Day 14: Extended Polymerization ---
//=============================================================================
// Principle:
type Pair = String
type PairCount = (Pair,BigInt)
type PairCounts = Map[Pair,BigInt]
type PairInserts = Map[Pair, String]

def parse(filename:String) = {
  val (t,i) = scala.io.Source.fromFile(filename).getLines.toList.span(!_.isEmpty)
  val pcs = t.head.sliding(2).toList.groupBy(identity).map{ case(k,vs) => (k, BigInt(vs.length))}
  val ins  = i.tail.map(_.split(" -> ")).map(a => a(0) -> a(1)).toMap
  (pcs,ins) }

def addInsert(pc:PairCount, pis:PairInserts) = {
  val (pair, count) = pc
  pis.get(pair) match {
    case Some(insert) => List((pair(0).toString + insert, count), (insert + pair(1).toString, count))
    case None         => List(pc) }}

def addInserts(pcs:PairCounts, pis:PairInserts) = pcs.toList.map(pc => addInsert(pc, pis)).flatten.groupMapReduce(_._1)(_._2)(_ + _)

def answer(steps:Int, pcs:PairCounts, pis:PairInserts) = {
  val finalPairCounts = (0 until steps).foldLeft(pcs)((acc,_) => addInserts(acc, pis))
  val letterCounts = finalPairCounts.toList
    .map{ case (p,c) => (p(1),c) }
    .groupMapReduce(_._1)(_._2)(_ + _)
  val counts = letterCounts.map(_._2)
  counts.max - counts.min }

def part1(i:(PairCounts, PairInserts)) = answer(10,i._1,i._2)
def part2(i:(PairCounts, PairInserts)) = answer(40,i._1,i._2)

//==ANSWERS====================================================================
println("Advent of Code 2021 --- Day 14: Extended Polymerization ---")
println(" part 1 : example : " + part1(parse("data/14e")))
println(" part 1 : actual : " + part1(parse("data/14")))
println(" part 2 : example : " + part2(parse("data/14e")))
println(" part 2 : actual : " + part2(parse("data/14")))
