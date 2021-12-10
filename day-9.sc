val wall = 9
type Heights = List[Array[Int]]


def points(g:grid) = (0 until g.rows).map(r => (0 until g.cols
def adjpoints(r:Int, c:Int) = Seq((r,c-1),(r+1,c),(r-1,c),(r,c+1))
def lowpoint(hs:Heights)(r:Int, c:Int):Boolean = hs(r)(c) match {
  case 9 => false
  case h => h < adjpoints(r,c).map{ case (a,b) => hs(a)(b)}.min
}
def lowpoints(g:Grid) =  

//=============================================================================
def readFrom(filename:String) = {
  val ls = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty).toArray
  ls.map(_.map(_.asDigit).toArray).toList
}
//=============================================================================
val inputs = Seq("data/day-9-example.txt", "data/day-9.txt")
  .map(readFrom(_))

