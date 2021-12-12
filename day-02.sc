//=============================================================================
// Advent of Code 2021 Day 2 (Dive)
//=============================================================================
// Principle: Higher Order Functions, pattern matching and type algebras 

case class Location(position:Int, depth:Int, aim:Int)
type Move = (String,Int)

def readFile(filename:String) = {
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

//==ANSWERS====================================================================
println("Advent of Code 2021 Day 2 (Dive)")
println(" part 1 : example : " + answer(noAim)(readFile("data/02e")))
println(" part 1 : actual  : " + answer(noAim)(readFile("data/02")))
println(" part 2 : example : " + answer(withAim)(readFile("data/02e")))
println(" part 2 : actual  : " + answer(withAim)(readFile("data/02")))
