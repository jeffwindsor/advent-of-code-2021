import scala.io.Source

object Input{
  def asLines(filename:String):List[String]=
    Source.fromFile(System.getProperty("user.dir") + "/src/github.com/jeffwindsor/advent-of-code-2021/inputs/"+filename).getLines
      .map(_.trim).toList

  def asNonEmptyLines(filename:String):List[String]=asLines(filename).filter(_.nonEmpty)
}
