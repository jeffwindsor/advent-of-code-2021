
type Grid = List[Array[Int]]

def part1(file:String) = {
  val g = readFrom(file)
  val rows = g.length
  val cols = g(0).length
  for{
    r <- 0 until rows
    c <- 0 until cols
    val height = g(r)(c)
    val minAdjHeight = List((r,c-1),(r+1,c),(r-1,c),(r,c+1))
      .filter{ case (a,b) => a >= 0 && a < rows && b >=0 && b < cols }
      .map{ case (a,b) => g(a)(b)}.min
    if height < minAdjHeight 
  } yield height
}.map(_ + 1).sum


def part2(file:String) = {
  val g = readFrom(file)
  val rows = g.length
  val cols = g(0).length
  var currentbasin = -2147483648
  for(r <- 0 until rows; c <- 0 until cols){
    if(g(r)(c) != 9){ 
      val points = List((r,c),(r,c-1),(r+1,c),(r-1,c),(r,c+1))
        .filter{ case (a,b) => a >= 0 && a < rows && b >=0 && b < cols }
        .filter{ case (a,b) => g(a)(b) != 9 }
      val minValue = points.map{ case (a,b) => g(a)(b) }.min
      if(minValue >= 0) {
        currentbasin += 1
        for(p <- points) g(p._1)(p._2) = currentbasin
      } else {
        for(p <- points) g(p._1)(p._2) = minValue
      }  
    }
  }
 //for(r <- 0 until rows){ 
 //  for(c <- 0 until cols) print(g(r)(c) + " | ")
 //  println
 //}


  g.flatten.filter(_ < 0)
   .groupMapReduce(identity)(_ => 1)(_ + _)
   .map(_._2).toList.sorted(Ordering[Int].reverse)
   .take(3).product
}

//=============================================================================
def readFrom(filename:String): Grid = {
  val filelines = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  filelines.map(_.map(_.asDigit).toArray).toList
}
//=============================================================================
val inputs = Seq("data/day-9-example.txt", "data/day-9.txt")
val funcs = Seq(part1(_), part2(_))
for(i <- inputs; f <- funcs) println(f(i))
