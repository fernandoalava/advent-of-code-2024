import scala.io.Source
import scala.collection.mutable
object Day11 {
  val testInput = "125 17"

  def getInput(): String = {
    Source.fromFile("src/main/resources/day11/input.txt").getLines().mkString
  }
  def parseInput(input: String): Vector[String] = {
    input.split(" ").toVector
  }

  type Key = (Long, Int)

  // This is not good, it totally breaks the idea of using FP, I need to figure out a better way to do this.
  var cache: mutable.Map[Key, Long] = mutable.Map.empty

  def blink(
      stone: Long,
      blinks: Int
  ): Long = {
    if (blinks == 0) {
      return 1L
    }
    val a = (stone, blinks)
    if (cache.contains(a)) {
      return cache.get(a).get
    }
    val nextValue = stone match {
      case 0L =>
        blink(1L, blinks - 1)
      case value if (value.toString().length() % 2 == 0) => {
        val v = value.toString()
        val middle = v.length() / 2
        val firstValue = v.substring(0, middle).toLong
        val secondValue = v
          .substring(middle)
          .toLong
        blink(
          firstValue,
          blinks - 1
        )
          + blink(
            secondValue,
            blinks - 1
          )
      }
      case value => {
        blink(value.toLong * 2024L, blinks - 1)
      }
    }
    cache ++= Map((stone, blinks) -> nextValue)
    return nextValue
  }

  def puzzle1(input: Vector[String], blinks: Int): Long = {
    input
      .map(_.toLong)
      .map(stone => blink(stone, blinks))
      .sum
  }
}

@main def mainDay11() = {
  println(Day11.puzzle1(Day11.parseInput(Day11.testInput), 75))
  println(Day11.puzzle1(Day11.parseInput(Day11.getInput()), 25))
  println(Day11.puzzle1(Day11.parseInput(Day11.getInput()), 75))
}
