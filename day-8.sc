//
//  Day 8: Seven Segment Search
//

type SignalPatterns = List[String]
type OutputValues = List[String]
case class Entry(ps:SignalPatterns, os:OutputValues)
//==============================================================================
def readFrom(filename:String) = {
  scala.io.Source
    .fromFile(filename)
    .getLines
    .map(_.split(Array('|')))
    .map(e => Entry(e(0).trim.split(' ').toList,e(1).trim.split(' ').toList))
}

val knowns = Map(2 -> 1, 4 -> 4, 3 -> 7, 7 -> 8)
def toDigit(output:String) = knowns.get(output.length)
def remap(os:OutputValues) = os.map(toDigit(_)).flatten
def easyDigits(es:Iterator[Entry]) = es.map(e => remap(e.os).length).sum


//==============================================================================
val inputs    = Seq("data/day-8-example.txt", "data/day-8.txt")
val functions = Seq(easyDigits(_))

for(i <- inputs; f <- functions) println(f(readFrom(i)))
