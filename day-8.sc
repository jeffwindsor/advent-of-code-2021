





//==============================================================================
// Effectful
//==============================================================================
def readFrom(filename:String) = {
  val filelines = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  filelines.split(',').map(_.toInt) 
}

val inputs = Seq("data/day-7-example.txt", "data/day-7.txt").map(readFrom(_))
val funcs = Seq(minimumCost(identity) _, minimumCost(terminal(_)) _) 
for(i <- inputs; f <- funcs) println(f(i))

