//=============================================================================
// Advent of Code 2021 --- Day 14: Extended Polymerization ---
//=============================================================================
// Principle:
type Template = String
type Result = String
type Inserts = Map[String, String]

def parse(filename:String) = {
  val inputs = scala.io.Source.fromFile(filename).getLines.span(!_.isEmpty)
  val template = inputs._1.toList.head
  val inserts  = inputs._2.filter(!_.isEmpty).map(_.split(" -> ")).map(a => (a(0), a(1))).toMap
  (template,inserts)
}

def applyInserts(template:Template, inserts:Inserts) = {
  template.sliding(2).foldLeft(template.head.toString)((acc,t) => inserts.get(t) match {
    case Some(i) => acc + i + t(1)
    case None    => acc + t(1)
  }) 
}
def answer(steps:Int, template:Template, inserts:Inserts) = { 
  val s = (0 until steps).foldLeft(template)((acc,_) => applyInserts(acc, inserts))
  val counts = s.groupMapReduce(identity)(_ => BigInt(1))(_ + _).map(_._2)
  counts.max - counts.min
} 

def part1(template:Template, inserts:Inserts) = answer(10, template, inserts)
def part2(template:Template, inserts:Inserts) = answer(40, template, inserts)

//==ANSWERS====================================================================
println("Advent of Code 2021 --- Day 14: Extended Polymerization ---")
val e = parse("data/14e")
println(" part 1 : example : " + part1(e._1, e._2))
//println(" part 2 : example : " + part2(e._1, e._2))

val a = parse("data/14")
println(" part 1 : actual  : " + part1(a._1,a._2))
