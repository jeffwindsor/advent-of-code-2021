case class Point(r:Int,c:Int){
  def +(that:Point): Point = Point(this.r + that.r, this.c + that.c)
  def neighbors: Seq[Point] = List(Point(0,1),Point(0,-1),Point(1,0),Point(-1,0)).map(p => p + this)
}