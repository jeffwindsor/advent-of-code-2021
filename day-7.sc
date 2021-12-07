// --- Day 7: The Treachery of Whales ---
//

def fuelCostByPosition(rangeOfCrabs:Seq[Int], crabs:Seq[Int]) = crabs.map(c => rangeOfCrabs.map(i => (i, math.abs(i - c)))).flatten
def totalFuelCostByPosition(fuelCostByPosition:Seq[(Int,Int)]) = fuelCostByPosition.groupMapReduce(t => t._1)(t => t._2)(_ + _)

def run(crabs:Seq[Int]) = { 
  val fcp = fuelCostByPosition(crabs.min to crabs.max, crabs) 
  val tfcp = totalFuelCostByPosition(fcp) 
  tfcp.minBy(t => t._2)._2
}

//==============================================================================
// Effectful
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
