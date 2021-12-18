import scala.io.Source

object Input{
  def asLines(filename:String):List[String]=Source.fromFile("/Users/jefwinds/src/github.com/jeffwindsor/advent-of-code-2021/inputs/"+filename).getLines.toList
  def asNonEmptyLines(filename:String):List[String]=asLines(filename).filter(_.nonEmpty)
}
