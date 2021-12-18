trait Result
case class Corrupted(score:BigInt) extends Result
case class Incomplete(score:BigInt) extends Result


val open  = Map(')'->'(', ']'->'[','}'->'{', '>'->'<')
val corruptscore = Map(')'-> 3, ']'-> 57,'}'-> 1197, '>'-> 25137)
val incompletescore = Map('('-> BigInt(1), '['-> BigInt(2),'{'-> BigInt(3), '<'-> BigInt(4))

def process(line:String):Result = {
  val s = new scala.collection.mutable.Stack[Char]
  val openChars = open.values.toList
  for(char <- line){
    if(openChars.contains(char)) s.push(char)
    else if(open(char) != s.pop) return Corrupted(corruptscore(char))
  }
  val total = s.toList.map(incompletescore(_))
    .fold(BigInt(0))((acc,x) => acc * 5 + x) 
  Incomplete(total)
}
def part1(lines:Seq[String]) = lines.map(process(_)).collect{ case Corrupted(s) => s }.sum
def part2(lines:Seq[String]) = { 
  val scores = lines.map(process(_)).collect{ case Incomplete(s) => s }.sorted
  scores(scores.length / 2)
}

//=============================================================================
def readFrom(filename:String) = {
  val ls = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty).toList
  ls }
//=============================================================================
val inputs = Seq("data/day-10-example.txt", "data/day-10.txt"
  ).map(readFrom(_))
for (input <- inputs) {
    println(part1(input))  
    println(part2(input))
}
