import shared.{Input, Output}
trait Result
case class Corrupted(score:BigInt) extends Result
case class Incomplete(score:BigInt) extends Result

val open  = Map(')'->'(', ']'->'[','}'->'{', '>'->'<')
val corruptScore = Map(')'-> 3, ']'-> 57,'}'-> 1197, '>'-> 25137)
val incompleteScore = Map('('-> BigInt(1), '['-> BigInt(2),'{'-> BigInt(3), '<'-> BigInt(4))

def process(line:String):Result = {
  val s = new scala.collection.mutable.Stack[Char]
  val openChars = open.values.toList
  for(char <- line){
    if(openChars.contains(char)) s.push(char)
    else if(open(char) != s.pop) return Corrupted(corruptScore(char))
  }
  val total = s.toList.map(incompleteScore(_))
    .fold(BigInt(0))((acc,x) => acc * 5 + x) 
  Incomplete(total)
}
def part1(filename:String) = Input.asNonEmptyLines(filename)
  .map(process).collect{ case Corrupted(s) => s }.sum
def part2(filename:String) = {
  val scores = Input.asNonEmptyLines(filename).map(process).collect{ case Incomplete(s) => s }.sorted
  scores(scores.length / 2) }

Output.printResults(10,part1)
Output.printResults(10,part2)
