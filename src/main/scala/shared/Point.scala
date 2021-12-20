package shared

case class Point(x:Int, y:Int){
  def +(that:Point): Point = Point(this.x + that.x, this.y + that.y)
  def neighbors: Seq[Point] = List(Point(0,1),Point(0,-1),Point(1,0),Point(-1,0)).map(p => p + this)
}