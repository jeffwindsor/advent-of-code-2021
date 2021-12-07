//
// Day 7: The Treachery of Whales
//

def intMax = 2147483647
def totalFuelCost(fuelCost:Int=>Int)(crabs:Iterable[Int])(position: Int) = 
  crabs.foldLeft(0){ case (total,crab) => total + fuelCost(math.abs(position - crab)) }
def minimumFuelCost(fuelCost:Int => Int)(crabs:Iterable[Int]) = 
  (crabs.min to crabs.max).foldLeft(intMax){ case (minTotal, position) => minTotal min totalFuelCost(fuelCost)(crabs)(position) }
def terminal(n:Int) = (n * (n + 1)) / 2

def run1 = minimumFuelCost(identity) _
def run2 = minimumFuelCost(distance => terminal(distance)) _

//==============================================================================
// Effectful
//==============================================================================
def readFrom(filename:String) = {
  val filelines = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  filelines.next.split(',').map(_.toInt) 
}

val files = Seq("data/day-7-example.txt", "data/day-7.txt")
val functions = Seq(run1(_), run2(_)) 
for(file <- files; function <- functions) println(function(readFrom(file)))
