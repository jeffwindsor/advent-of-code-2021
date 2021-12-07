//
// Day 7: The Treachery of Whales
//
type CostF    = Int => Int
type Crab     = Int
type Crabs    = Iterable[Crab]
type Position = Int

def terminal(n:Int) = (n * (n + 1)) / 2
def costAtPosition(f:CostF)(c:Crab)(p:Position) = f(math.abs(p - c))
def totalCostAtPosition(f:CostF)(cs:Crabs)(p:Position) = cs.foldLeft(0){ case (acc,c) => acc + costAtPosition(f)(c)(p) }
def minimumCost(f:CostF)(cs:Crabs) = { 
  val positions = (cs.min to cs.max)
  positions.foldLeft(2147483647){ case (acc, p) => acc min totalCostAtPosition(f)(cs)(p) }
}

//==============================================================================
// Effectful
//==============================================================================
def readFrom(filename:String) = {
  val filelines = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  filelines.next.split(',').map(_.toInt) 
}

val files = Seq("data/day-7-example.txt", "data/day-7.txt")
val functions = Seq(minimumCost(identity) _, minimumCost(terminal(_)) _) 
for(file <- files; function <- functions) println(function(readFrom(file)))
