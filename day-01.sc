//
// DAY 1: Sonar Sweep
//
def readFrom(filename:String) = {
  scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
    .map(_.toInt).toSeq
}

// PART 1 : count the number of increases between measurements
def count_increases(inputs: Seq[Int]) = 
  inputs.sliding(2).count { case Seq(a,b) => a < b }

// PART 2 : reduce measurement to sums of triples
//          then count the number of increases between sums
def count_sum_increases(inputs: Seq[Int]) = 
  count_increases(inputs.sliding(3).map(_.sum).toSeq)

//==PRINT ANSWERS==================================================================================
for(input <- Seq("data/day-1-example.txt").map(readFile(_))){
  count_increases(input)
  count_sum_increases(input)
}
