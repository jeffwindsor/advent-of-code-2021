// Laternfish
//
import scala.annotation.tailrec

val restart = 6
val start   = restart + 2
val spawn   = 0
val iterationsForPart1  = 80
val iterationsForPart2  = 256

def readFrom(filename:String) = {
  val filelines = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  filelines.next.split(',').map(_.toInt)
}

type NumberOfFish = BigInt
type Generation   = Int
type Population   = Map[Generation,NumberOfFish]   

def run(iterations:Int, inputs:Iterable[Generation]):NumberOfFish = {
  val fishCountPerGeneration = inputs.groupMapReduce(identity)(_ => 1)(_ + _) 
  val population = fishCountPerGeneration.map{ case (k,v) => (k, BigInt(v)) } 
  runIterations(iterations, population).values.sum }

@tailrec
def runIterations(i:Int, p:Population):Population = 
  if(i == 0) p else runIterations( i - 1, iterate(p) )

def iterate(g:Generation):Generation = if(g == spawn) restart else g - 1
def iterate(p:Population):Population = {
  val newPopulation = p.groupMapReduce { case (g, _) => iterate(g) }(p => p._2)(_ + _)
  p.get(spawn) match {
    case Some(numOfFish) => newPopulation + (start -> numOfFish) 
    case None => newPopulation } }

//==============================================================================
// Print Answers
//==============================================================================
val files = Seq("data/day-6-example.txt", "data/day-6.txt")
val functions = Seq(run(iterationsForPart1, _), run(iterationsForPart2,_)) 
for(file <- files; function <- functions) println(function(readFrom(file)))

