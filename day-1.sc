// Sonar Sweep
//
def readFrom(filename:String) = {
  val filelines = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  filelines.map(_.toInt).toSeq
}

//  count the number of increases between measurements
def count_increases(inputs: Seq[Int]) = 
  inputs.sliding(2).count { case Seq(a,b) => a < b }

//  consider sums of a three-measurement sliding window.
//  count the of increases between three-measurement sums
def count_increase_between_windows(inputs: Seq[Int]) = 
  count_increases(inputs.sliding(3).map(_.sum).toSeq)

// Answers
for(file <- Seq("data/day-1-example.txt", "data/day-1.txt"); 
    function <- Seq(count_increases(_), count_increase_between_windows(_))){
       println(function(readFrom(file)))
}
