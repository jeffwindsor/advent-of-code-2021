package shared

object Output:
  type AnswerFunc = String => Any
  def printResults(day:Int, part1:AnswerFunc, part2:AnswerFunc):Unit =
    val actual  = if(day < 10) s"0$day" else s"$day"
    val example = actual+"e"
    println(s"Day $day")
    println(" part 1 : example : "+part1(example))
    println(" part 1 : actual  : "+part1(actual))
    println(" part 2 : example : "+part2(example))
    println(" part 2 : actual  : "+part2(actual))
  
