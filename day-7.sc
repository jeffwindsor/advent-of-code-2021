//
// Day 7: The Treachery of Whales
//

def intMax = 2147483647
def totalFuelCost(crabs:Iterable[Int])(position: Int) = 
  crabs.foldLeft(0){ case (total,crab) => total + math.abs(position - crab) }
def run(crabs:Iterable[Int]) = 
  (crabs.min to crabs.max).foldLeft(intMax){ case (minTotal, position) => minTotal min totalFuelCost(crabs)(position) }

//==============================================================================
// Effectful
//==============================================================================
def readFrom(filename:String) = {
  val filelines = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  filelines.next.split(',').map(_.toInt) 
}

val files = Seq("data/day-7-example.txt", "data/day-7.txt")
val functions = Seq(run(_)) 
for(file <- files; function <- functions) println(function(readFrom(file)))
