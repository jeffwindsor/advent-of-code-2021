import shared.{Input, Output}

def data(filename:String):List[Int] = Input.asNonEmptyLines(filename).map(_.toInt)

def count(inputs: List[Int]) = inputs.sliding(2).count{ case Seq(a,b) => a < b }
def part1(f:String) = count(data(f))
def part2(f:String) = count(data(f).sliding(3).map(_.sum).toList)

Output.printResults(1,part1)
Output.printResults(1,part2)
