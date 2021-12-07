// Laternfish
//
import scala.annotation.tailrec

type NumberOfFish = BigInt
type Generation   = Int
type Population   = Map[Generation,NumberOfFish]   

def run(iterations:Int, inputs:Iterable[Generation]):NumberOfFish = {
  val population = inputs.groupMapReduce(identity)(_ => BigInt(1))(_ + _) 
  runIterations(iterations, population).values.sum }

@tailrec
def runIterations(i:Int, p:Population):Population = 
  if(i == 0) p else runIterations( i - 1, next(p) )

def next(p:Population):Population = {
  val newPopulation = p.groupMapReduce { 
    case (g, _) => if(g == 0) 6 else g - 1  }(f => f._2)(_ + _)
  p.get(0) match {
    case Some(numOfFish) => newPopulation + (8 -> numOfFish) 
    case None => newPopulation } }

//==============================================================================
// Print Answers
//==============================================================================
def readFrom(filename:String) = {
  val filelines = scala.io.Source.fromFile(filename).getLines.filter(_.nonEmpty)
  filelines.next.split(',').map(_.toInt) }

val files = Seq("data/day-6-example.txt", "data/day-6.txt")
val functions = Seq(run(80, _), run(256,_)) 
for(file <- files; function <- functions) println(function(readFrom(file)))

