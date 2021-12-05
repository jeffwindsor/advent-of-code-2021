//  count the number of increases between measurements
def count_increases(inputs: Seq[Int]) = inputs
  .sliding(2)
  .count { case Seq(a,b) => a < b }

//  consider sums of a three-measurement sliding window.
//  count the of increases between three-measurement sums
def count_increase_between_windows(inputs: Seq[Int]) = count_increases( 
  inputs
    .sliding(3)
    .map(_.sum)
    .toSeq)



// get some answers
def read(filepath:String): Seq[Int] = scala.io.Source
  .fromFile(filepath)
  .getLines
  .map(_.toInt)
  .toSeq

val test_inputs = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
val submission_inputs = read("inputs/day-1.txt")

println(count_increases(test_inputs))                       //7
println(count_increases(submission_inputs))                  //1233
println(count_increase_between_windows(test_inputs))         //5
println(count_increase_between_windows(submission_inputs))   //1275
