import shared.{Input, Output}
type CostF    = Int => Int
type Crab     = Int
type Crabs    = Iterable[Crab]
type Position = Int

def data(filename:String) = Input.asNonEmptyLines(filename).head.split(',').map(_.toInt)
def terminal(n:Int) = (n * (n + 1)) / 2
def costAtPosition(f:CostF)(c:Crab)(p:Position) = f(math.abs(p - c))
def totalCostAtPosition(f:CostF)(cs:Crabs)(p:Position) = cs.foldLeft(0){ case (acc,c) => acc + costAtPosition(f)(c)(p) }
def minimumCost(f:CostF, cs:Crabs) = (cs.min to cs.max)
  .foldLeft(2147483647){ case (acc, p) => acc min totalCostAtPosition(f)(cs)(p) }
def part1(f:String) = minimumCost(identity, data(f))
def part2(f:String) = minimumCost(terminal(), data(f))

Output.printResults(7, part1, part2)
