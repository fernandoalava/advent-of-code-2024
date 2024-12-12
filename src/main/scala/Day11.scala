import scala.io.Source
object Day11 {
  val testInput = "125 17"

  def getInput(): String = {
    Source.fromFile("src/main/resources/day11/input.txt").getLines().mkString
  }
  def parseInput(input: String): Vector[String] = {
    input.split(" ").toVector
  }

  def transformStore(stone: String): Vector[String] = {
    stone match
      case "0" => Vector("1")
      case value if (value.length() % 2 == 0) => {
        val middle = value.length() / 2
        val firstValue = BigInt(value.substring(0, middle))
        val secondValue = BigInt(value
              .substring(middle)
        )
        Vector(
          s"${firstValue}",
          s"${secondValue}"
        )
      }
      case value => {
        val result = BigInt(value) * BigInt(2024)
        Vector(s"${result}")
      }
  }

  def puzzle1(input: Vector[String], blinks: Int): Int = {
    val stones = blinks match
      case 0 => input
      case _ =>
        (0 until blinks).foldLeft(input)((newInput, blink) => newInput.flatMap(transformStore))
    stones.size
  }
}

@main def mainDay11() = {
//   println(Day11.puzzle1(Day11.parseInput(Day11.testInput), 25))
//   println(Day11.puzzle1(Day11.parseInput(Day11.getInput()), 25))
  println(Day11.puzzle1(Day11.parseInput(Day11.getInput()), 75))
}
