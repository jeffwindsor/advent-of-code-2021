import scala.annotation.tailrec

val on  = '1'
val off = '0'

def data(filename:String) = Input.asNonEmptyLines(filename)

def bitsToDecimal(bits:Seq[Char]) = bits
  .reverse.zipWithIndex                           // reverse bit order so that index with match power of 2
  .filter(_._1 == on)                             // filter to only on bits
  .map { case (_,power) => math.pow(2, power) }   // each bit to decimal value of 2^n
  .sum.toInt                                      // sum to to get base 10

def mostCommon(readings: List[String]):List[Char] = readings
  .transpose                                                // pivot
  .map(_.count(_ == on))                                    // count the on bits in column
  .map(c => if (c >= readings.length - c ) on else off)     // set most common digit, tie goes to on bit

def leastCommon(readings:List[String]):List[Char] =
  mostCommon(readings).map(c => if (c == on) off else on)   // invert most common

@tailrec
def matchBits(f:List[String]=>List[Char], readings: List[String], index:Int = 0): String = readings match {
  case List(answer) => answer                                                       // quit on single value
  case _            => { val filterBit = f(readings)(index)                         // get bit to filter readings at index
                         val filtered  = readings.filter(_(index) == filterBit)     // filter reading to only those with filterBit at index location
                         matchBits(f, filtered, index+1 )}}                         // recurse with filtered readings and increment index

def powerConsumption(readings: List[String]) = {
  val gammaRate   = bitsToDecimal(mostCommon(readings))
  val epsilonRate = bitsToDecimal(leastCommon(readings))
  gammaRate * epsilonRate }

def lifeSupport(readings: List[String]) = {
  val co2 = bitsToDecimal(matchBits(mostCommon,readings))
  val o2  = bitsToDecimal(matchBits(leastCommon,readings))
  co2 * o2 }

def part1(f:String) = powerConsumption(data(f))
def part2(f:String) = lifeSupport(data(f))
Output.printResults(3, part1, part2)
