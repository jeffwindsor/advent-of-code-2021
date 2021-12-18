import scala.annotation.tailrec
type Fish = BigInt
type Age  = Int

def data(filename:String): Iterable[Age] =
  Input.asNonEmptyLines(filename).head.split(',').map(_.toInt)

def run(iterations:Int, inputs:Iterable[Age]):Fish = {
  @tailrec def inner(i:Int, p:Map[Age,Fish]):Map[Age,Fish] = if(i==0) p else inner(i-1, next(p))
  val population = inputs.groupMapReduce(identity)(_ => BigInt(1))(_ + _)
  inner(iterations, population).values.sum }

def next(p:Map[Age,Fish]):Map[Age,Fish] = {
  val newPopulation = p.groupMapReduce{ case(g,_)=>if(g==0) 6 else g-1 }(f => f._2)(_+_)
  p.get(0) match { case Some(numOfFish) => newPopulation + (8 -> numOfFish)
                   case None            => newPopulation } }
def part1(f:String) = run(80,data(f))
def part2(f:String) = run(256,data(f))
Output.printResults(6, part1, part2)
