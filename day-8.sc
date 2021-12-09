//
//  Day 8: Seven Segment Search
//                                            digit    wires    total on wires
//  0:      1:      2:      3:      4:            1 -> ..c..f.  2   unique length
//   aaaa    ....    aaaa    aaaa    ....         7 -> a.c..f.  3   unique length
//  b    c  .    c  .    c  .    c  b    c        4 -> .bcd.f.  4   unique length
//  b    c  .    c  .    c  .    c  b    c        8 -> abcdefg  7   all
//   ....    ....    dddd    dddd    dddd         
//  e    f  .    f  e    .  .    f  .    f        2 -> a.cde.g  5   none
//  e    f  .    f  e    .  .    f  .    f        3 -> a.cd.fg  5   fully contains(1,7)
//   gggg    ....    gggg    gggg    ....         5 -> ab.d.fg  5   none, but fully contained by 6
//                                                
//  5:      6:      7:      8:      9:            6 -> ab.defg  6   fully contains(none | 5, )
//   aaaa    aaaa    aaaa    aaaa    aaaa         0 -> abc.efg  6   fully contains(1,7)
//  b    .  b    .  .    c  b    c  b    c        9 -> abcd.fg  6   fully contains(4,7 | 3, 5)
//  b    .  b    .  .    c  b    c  b    c        
//   dddd    dddd    ....    dddd    dddd
//  .    f  e    f  .    f  e    f  .    f
//  .    f  e    f  .    f  e    f  .    f
//   gggg    gggg    ....    gggg    gggg

//==============================================================================
def readFrom(filename:String) = 
  scala.io.Source.fromFile(filename).getLines
    .map(line => line.split('|')
      .map(_.trim.split(' ').toSet).toList).toList

def part_1_answer(file:String) = readFrom(file)
  .map(_(1).map(_.size))
  .flatten
  .filter(Set(2, 3, 4, 7).contains(_))
  .length

def lengthToDigits(byLength:Map[Int, Set[String]]) = {
  Map(1 -> byLength(2), 4 -> byLength(4), 7 -> byLength(3), 8 -> byLength(7))
}

def part_2_answer(file:String) = readFrom(file)
  .map(e => lengthToDigits(e(0).groupBy(_.size)))

//==============================================================================
val inputs    = Seq("data/day-8-example.txt") //, "data/day-8.txt")
val functions = Seq(part_1_answer(_), part_2_answer(_)) //(s:String) => readFrom(s).toList) 
for(i <- inputs; f <- functions) println(f(i))
