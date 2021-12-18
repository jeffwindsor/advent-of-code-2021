//
//  Day 8: Seven Segment Search
//                                            digit    wires    total on wires
//  0:      1:      2:      3:      4:            1 -> ..c..f.  2   unique length
//   aaaa    ....    aaaa    aaaa    ....         4 -> .bcd.f.  4   unique length
//  b    c  .    c  .    c  .    c  b    c        7 -> a.c..f.  3   unique length
//  b    c  .    c  .    c  .    c  b    c        8 -> abcdefg  7   all
//   ....    ....    dddd    dddd    dddd         
//  e    f  .    f  e    .  .    f  .    f        2 -> a.cde.g  5   none
//  e    f  .    f  e    .  .    f  .    f        3 -> a.cd.fg  5   fully contains(1,7)
//   gggg    ....    gggg    gggg    ....         5 -> ab.d.fg  5   none, but fully contained by 6
//                                                
//  5:      6:      7:      8:      9:            6 -> ab.defg  6   fully contains(5)
//   aaaa    aaaa    aaaa    aaaa    aaaa         0 -> abc.efg  6   fully contains(1,7)
//  b    .  b    .  .    c  b    c  b    c        9 -> abcd.fg  6   fully contains(1,4,7,3,5)
//  b    .  b    .  .    c  b    c  b    c        
//   dddd    dddd    ....    dddd    dddd
//  .    f  e    f  .    f  e    f  .    f
//  .    f  e    f  .    f  e    f  .    f
//   gggg    gggg    ....    gggg    gggg
//

type Number = Int
type Pattern = Set[Char]
type Patterns = Iterable[Pattern]

def data(filename:String) = Input.asLines(filename)
  .map(line => line.split('|')
    .map(_.trim.split(' ').map(_.toSet).toList).toList)

def pattern2Number(ps:Patterns): Map[Pattern, Number] = {
  val pss = ps.groupBy(_.size)
  def find(s:Int, includes:Patterns, excludes:Patterns):Pattern =
    pss(s).filter(p => includes.forall(_.subsetOf(p)))
         .filter(p => !excludes.exists(_ == p)).toList.head
  val one   = pss(2).head
  val four  = pss(4).head
  val seven = pss(3).head
  val eight = pss(7).head
  val nine  = find(6, List(one,four,seven), List())
  val zero  = find(6, List(one,seven), List(nine))
  val six   = find(6, List(), List(zero,nine))
  val three = find(5, List(one,seven), List())
  val five  =  pss(5).filter(p => p.subsetOf(six)).toList.head 
  val two   = find(5, List(), List(zero,one,three,four,five,six,seven,eight,nine))
  Map(zero->0,one->1,two->2,three->3,four->4,five->5,six->6,seven->7,eight->8, nine->9)
}

def part1(file:String) = data(file)
  .flatMap(_ (1).map(_.size))
  .count(Set(2, 3, 4, 7).contains)

def part2(file:String) = data(file).map{ e =>
    val p2n = pattern2Number(e.head)
    e(1).map(p2n(_)).mkString.toInt }.sum

Output.printResults(8, part1, part2)