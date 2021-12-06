// Hydrothermal Vents

case class Point(x:Int, y:Int)
case class Line(a:Point, b:Point){
  val dx = math.signum(b.x - a.x)
  val dy = math.signum(b.y - a.y)
  val numberOfPoints = math.max(math.abs(a.x - b.x), math.abs(a.y - b.y)) 
  val isHoV = dx * dy == 0
  val points = (0 to numberOfPoints).map(i => Point(a.x + dx * i, a.y + dy * i))
}

def readFrom(filename:String) = {
  val filelines = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  val numbers = filelines.map(_.split(Array(',',' ','-','>')).filter(!_.isEmpty).map(_.toInt))
  numbers.map(line => Line(Point(line(0),line(1)),Point(line(2),line(3))))
}

def answer(lines:Iterator[Line]) = lines
  .flatMap(_.points)              // collect all integer points on all lines
  .toSeq.groupBy(a => a)          // group by points
  .values.count(_.size > 1)       // get a count of points that exist more than once in list 

def hovOnly(lines:Iterator[Line]) = answer(lines.filter(_.isHoV))

// Answers
for(file <- Seq("data/day-5-example.txt", "data/day-5.txt"); 
     function <- Seq(hovOnly(_), answer(_))){
  val output = function(readFrom(file))
  println(output)
  //for(o <- output){ println(o) }
  //println(output.size)
}
