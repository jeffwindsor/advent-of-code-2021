// --- Day 7: The Treachery of Whales ---
//

def run(crabs:Seq[Int]) = { 
  val rangeOfCrabs = crabs.min to crabs.max
  val fuelCostByPosition = crabs.map(c => rangeOfCrabs.map(i => (i, math.abs(i - c))))
  val totalFuelCostByPosition = fuelCostByPosition.flatten.groupMapReduce(t => t._1)(t => t._2)(_ + _)
  val minFuelCostPosition = totalFuelCostByPosition.minBy(t => t._2)

  for(l <- fuelCostByPosition) printColumns(l) 
  printColumns(totalFuelCostByPosition.toList.sorted)

  minFuelCostPosition._2
}

//==============================================================================
// Print Answers
//==============================================================================
def readFrom(filename:String) = {
  val filelines = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  filelines.next.split(',').map(_.toInt) 
}

val files = Seq("data/day-7-example.txt")//, "data/day-7.txt")
val functions = Seq(run( _)) 
for(file <- files; function <- functions) println(function(readFrom(file)))
//==============================================================================



def printColumns(items: Iterable[(Int,Int)]) = {  
  for(p <- items){
    if(p._2 < 10) print("  " + p._2) else print( " " + p._2)
  }
  println
} 
