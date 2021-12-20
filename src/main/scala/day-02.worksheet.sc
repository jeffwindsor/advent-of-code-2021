import shared.{Input, Output}

case class Location(position:Int, depth:Int, aim:Int)
type Move = (String,Int)

def data(filename:String): List[(String, Int)] =
  Input.asNonEmptyLines(filename).map(_.split(' ')).map(a => (a(0), a(1).toInt))

def noAim(location:Location, move:Move):Location = move match {
    case ("forward", x) => Location(location.position + x, location.depth, 0)
    case ("down", x)    => Location(location.position, location.depth + x, 0)
    case ("up", x)      => Location(location.position, location.depth - x, 0)
    case _              => location
  }

def withAim(location:Location, move:Move):Location = move match {
    case ("forward", x) => Location(location.position + x, location.depth + (location.aim * x), location.aim)
    case ("down", x)    => Location(location.position, location.depth, location.aim + x)
    case ("up", x)      => Location(location.position, location.depth, location.aim - x)
    case _              => location
  }

def answer(movement:(Location,Move) => Location, moves: Iterable[Move]): Int = {
  val location = moves.foldLeft(Location(0,0,0))(movement)
  location.position * location.depth
}

def part1(f:String) = answer(noAim, data(f))
def part2(f:String) = answer(withAim, data(f))
Output.printResults(2,part1,part2)
