import scala.io.Source
import scala.util.matching.Regex.Match

@main def day3(): Unit = {

  val operationMatcher =
    """mul\((?<factor>\d{1,3}),(?<multiplier>\d{1,3})\)""".r
  val dontAndDoMatcher = """don't().*?(?=do\(\))|(?<=don't\(\)).*""".r

  def getInput: List[String] = {
    Source
      .fromFile("src/main/resources/day3/input.txt")
      .getLines()
      .toList
  }

  def createOperationPairs(m: Match): (Integer, Integer) =
    (Integer.valueOf(m.group("factor")), Integer.valueOf(m.group("multiplier")))

  def sumProducts(pairs: List[(Integer, Integer)]): Integer =
    pairs.map(v => v._1 * v._2).fold(0)(_ + _)

  def puzzle1(): Integer =
    sumProducts(
      getInput
        .flatMap(
          operationMatcher
            .findAllMatchIn(_)
            .map(createOperationPairs)
        )
    )

  def puzzle2(): Integer = {
    sumProducts(
      operationMatcher
        .findAllMatchIn(dontAndDoMatcher.replaceAllIn(getInput.mkString, ""))
        .map(createOperationPairs)
        .toList
    )
  }
  
  println(puzzle1())
  println(puzzle2())
}
