package shared

object Output:
  type AnswerFunc = String => Any
  def printResults(day:Int, f:AnswerFunc):Unit =
    val actual  = if(day < 10) s"0$day" else s"$day"
    val example = actual+"e"
    println(example + ": " + f(example))
    println(actual  + ": " + f(actual))