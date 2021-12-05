val test_inputs = List("forward 5","down 5","forward 8", "up 3","down 8", "forward 2")
type Location = (Int,Int,Int)
type Move = (String,Int)
def moves(inputs: List[String]) = 
  inputs.map(_.split(' '))
  .map(a => (a(0), a(1).toInt))

def noAim(location:Location, move:Move) = move match {
    case ("forward", x) => (location._1 + x, location._2, 0)
    case ("down", x)    => (location._1, location._2 + x, 0)
    case ("up", x)      => (location._1, location._2 - x, 0)
    case _              => location
  }

def withAim(location:Location, move:Move) = move match {
    case ("forward", x) => (location._1 + x, location._2 + (location._3 * x), location._3)
    case ("down", x)    => (location._1, location._2, location._3 + x)
    case ("up", x)      => (location._1, location._2, location._3 - x)
    case _              => location
  }

def answer(movement:(Location,Move) => Location)(inputs: List[String]): Int = { 
  val location = moves(inputs).foldLeft((0,0,0))(movement)
  location._1 * location._2
}

println("Part1/Test: " + answer(noAim)(test_inputs))
println("Part2/Test: " + answer(withAim)(test_inputs))
