import shared.*

val costFloor   = 0
val costCeiling = 2147483647

def data(filename:String) = Input.asNonEmptyLines(filename).head.split(',').map(_.toInt)

def costAtPosition(cost:Int, position:Int) = math.abs(position - cost)
def triangleCostAtPostion(cost:Int, position:Int) = costAtPosition(cost,position).triangleNumber

def totalCostAtPosition(getCost:(Int,Int)=>Int, costs:Iterable[Int], position:Int) =
  costs.foldLeft(costFloor){ case (total, c) => total + getCost(c, position) }
def minimumCost(getCost:(Int,Int)=>Int, costs:Iterable[Int]) = 
  (costs.min to costs.max).foldLeft(costCeiling){ 
    case (currentMin, p) => currentMin min totalCostAtPosition(getCost,costs,p)
  }

def part1(f:String) = minimumCost(costAtPosition, data(f))
def part2(f:String) = minimumCost(triangleCostAtPostion, data(f))

Output.printResults(7, part1, part2)
