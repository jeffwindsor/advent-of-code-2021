// Laternfish
//
import scala.annotation.tailrec

type Fish = Int
type Fishes = List[Int]

def cycle(fish:Fish):Fish = fish match{
  case 0 => 6
  case n => n - 1 }

def cycle(fishes:Fishes):Fishes = {
  val babies = fishes.filter(_ == 0).map(_ => 8)
  val cycled = fishes.map(cycle(_))
  cycled ++ babies }

@tailrec
def runCycles(cycles:Int, fishes:Fishes): Fishes = cycles match {
  case 0 => fishes
  case n => runCycles(n - 1, cycle(fishes)) }

def run(fishes:Fishes) = runCycles(80,fishes).length

// Reads
def readFrom(filename:String) = {
  val filelines = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  filelines.next.split(',').map(_.toInt).toList
}

// Answers
val files = Seq("data/day-6-example.txt", "data/day-6.txt")
val functions = Seq(run(_)) 
for(file <- files; function <- functions) println(function(readFrom(file)))

