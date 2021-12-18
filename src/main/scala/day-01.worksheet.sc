def data(filename:String) = Input.asNonEmptyLines(filename).map(_.toInt)

def count(inputs: Iterable[Int]) = inputs.sliding(2).count { case Seq(a,b) => a < b }
def part1(f:String) = count(data(f))
def part2(f:String) = count(data(f).sliding(3).map(_.sum).toSeq)

Output.printResults(1, part1,part2)
