// Dive!
//
case class Location(position:Int, depth:Int, aim:Int)
type Move = (String,Int)

def readFrom(filename:String) = {
  val filelines = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  filelines.map(_.split(' ')).map(a => (a(0), a(1).toInt)).toSeq
}

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

def answer(movement:(Location,Move) => Location)(moves: Seq[Move]): Int = { 
  val location = moves.foldLeft(Location(0,0,0))(movement)
  location.position * location.depth
}

// Answers
for(file <- Seq("data/day-2-example.txt", "data/day-2.txt"); 
     function <- Seq(answer(noAim)(_), answer(withAim)(_)))
    {
       val moves = readFrom(file)
       val output = function(moves)
       println(output)
    }
