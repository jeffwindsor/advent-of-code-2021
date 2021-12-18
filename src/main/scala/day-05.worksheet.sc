case class Point(x:Int, y:Int)
case class Line(a:Point, b:Point)

def data(filename:String): List[Line] = Input.asNonEmptyLines(filename)
  .map(_.split(Array(',',' ','-','>')).filter(_.nonEmpty).map(_.toInt))
  .map(line => Line(Point(line(0),line(1)),Point(line(2),line(3))))

def dx(l:Line) = math.signum(l.b.x - l.a.x)
def dy(l:Line) = math.signum(l.b.y - l.a.y)
def points(l:Line) = (0 to math.max(math.abs(l.a.x - l.b.x), math.abs(l.a.y - l.b.y)))
    .map(i => Point(l.a.x + dx(l) * i, l.a.y + dy(l) * i))
def isHoV(l:Line) = dx(l) * dy(l) == 0
def count(lines:List[Line]) = lines.flatMap(points).groupBy(a => a).values.count(_.size > 1)
def part1(f:String) = count(data(f).filter(isHoV))
def part2(f:String) = count(data(f))

Output.printResults(5, part1, part2)
