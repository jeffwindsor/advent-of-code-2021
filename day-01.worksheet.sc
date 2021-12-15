//=============================================================================
// Advent of Code 2021 Day 1 (Sonar Sweep)
//=============================================================================
// Principle: zip, windowing, sliding of iterables ??? 

def readFile(filename:String) = {
  scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
    .map(_.toInt).toSeq }

// PART 1 : count the number of increases between measurements
def count_increases(inputs: Seq[Int]) = 
  inputs.sliding(2).count { case Seq(a,b) => a < b }

// PART 2 : reduce measurement to sums of triples
//          then count the number of increases between sums
def count_sum_increases(inputs: Seq[Int]) = 
  count_increases(inputs.sliding(3).map(_.sum).toSeq)

//==ANSWERS====================================================================
println("Advent of Code 2021 Day 1 (Sonar Sweep)")
println(" part 1 : example : " + count_increases(readFile("data/01e")))
println(" part 1 : actual  : " + count_increases(readFile("data/01")))
println(" part 2 : example : " + count_sum_increases(readFile("data/01e")))
println(" part 2 : actual  : " + count_sum_increases(readFile("data/01")))
