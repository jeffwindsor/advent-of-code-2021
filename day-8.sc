//
//  Day 8: Seven Segment Search
//                                            digit    wires    total on wires
//  0:      1:      2:      3:      4:            1 -> ..c..f.  2   
//   aaaa    ....    aaaa    aaaa    ....         7 -> a.c..f.  3
//  b    c  .    c  .    c  .    c  b    c        4 -> .bcd.f.  4
//  b    c  .    c  .    c  .    c  b    c        2 -> a.cde.g  5
//   ....    ....    dddd    dddd    dddd         3 -> a.cd.fg  5
//  e    f  .    f  e    .  .    f  .    f        5 -> ab.d.fg  5
//  e    f  .    f  e    .  .    f  .    f        6 -> ab.dsfg  6
//   gggg    ....    gggg    gggg    ....         0 -> abc.efg  6
//                                                9 -> abcd.fg  6
//  5:      6:      7:      8:      9:            8 -> abcdefg  7
//   aaaa    aaaa    aaaa    aaaa    aaaa
//  b    .  b    .  .    c  b    c  b    c
//  b    .  b    .  .    c  b    c  b    c
//   dddd    dddd    ....    dddd    dddd
//  .    f  e    f  .    f  e    f  .    f
//  .    f  e    f  .    f  e    f  .    f
//   gggg    gggg    ....    gggg    gggg

type SignalPatterns = List[String]
type OutputValues = List[String]
case class Entry(ps:SignalPatterns, os:OutputValues)
//==============================================================================
def readFrom(filename:String) = {
  def values(vs:String) = vs.trim.split(' ').map(_.sorted).toList
  def entry(e:Array[String]) = Entry(values(e(0)),values(e(1)))
  scala.io.Source.fromFile(filename).getLines
    .map(line => entry(line.split('|')))
}

val knowns = Map(2 -> 1, 4 -> 4, 3 -> 7, 7 -> 8)
def toDigit(output:String) = knowns.get(output.length)
def remap(os:OutputValues) = os.map(toDigit(_)).flatten
def part_1_answer(file:String) = readFrom(file).map(e => remap(e.os).length).sum

def part_2_answer(file:String) = readFrom(file).toList 

//==============================================================================
val inputs    = Seq("data/day-8-example.txt")//, "data/day-8.txt")
val functions = Seq(part_1_answer(_), part_2_answer(_)) //(s:String) => readFrom(s).toList) 
for(i <- inputs; f <- functions) println(f(i))
