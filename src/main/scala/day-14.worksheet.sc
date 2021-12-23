import shared.{Input, Output}
type Pair = String
type PairCount = (Pair,BigInt)
type PairCounts = Map[Pair,BigInt]
type PairInserts = Map[Pair, String]

def dataPairCounts(filename:String) = 
  Input.asNonEmptyLines(filename).head.sliding(2).toList.groupBy(identity).map{ case(k,vs) => (k, BigInt(vs.length))}
  
def dataInserts(filename:String) = 
  Input.asNonEmptyLines(filename).tail.map(_.split(" -> ")).map(a => a(0) -> a(1)).toMap

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

def part1(f:String) = answer(10,dataPairCounts(f),dataInserts(f))
def part2(f:String) = answer(40,dataPairCounts(f),dataInserts(f))

Output.printResults(14,part1)
Output.printResults(14,part2)
