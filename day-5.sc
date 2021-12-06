// Hydrothermal Vents

case class Point(x:Int, y:Int)
case class Line(a:Point, b:Point)

def readFrom(filename:String) = {
  val strings = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  val numbers = strings.map(_.split(Array(',',' ','-','>')).filter(!_.isEmpty).map(_.toInt))
  val lines   = numbers.map(line => Line(Point(line(0),line(1)),Point(line(2),line(3))))
  lines
}

for(line <- readFrom("data/day-5-example.txt")) {
  println(line)
}
