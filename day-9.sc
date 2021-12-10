val wall = 9
type HeightGrid = List[Array[Int]]
case class Grid(rows:Int, cols:Int, hs:HeightGrid)


//def adjpoints(r:Int, c:Int) = Seq((r,c-1),(r+1,c),(r-1,c),(r,c+1))
//    .filter{ case (a,b) => a >= 0 && a < rows && b >=0 && b < cols }
//def lowpoint(g:Grid)(r:Int, c:Int):Boolean = g(r)(c) match {
//  case 9 => false
//  case h => h < adjpoints(r,c).map{ case (a,b) => g(a)(b)}.min
//}


//=============================================================================
def cappedRow(row:String) = Array(wall) ++ row.map(_.asDigit).toArray ++ Array(wall)
def wallRow(length:Int) = (0 until length).map(_ => 9).toArray
def readFrom(filename:String) = {
  val filelines = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  val grid = wallRow() :+ filelines.map(cappedRow(_)).toList :+ wallRow()
  val rows = grid.length
  val cols = grid(0).length

}
//=============================================================================
val inputs = Seq("data/day-9-example.txt", "data/day-9.txt").map(readFrom(_))

