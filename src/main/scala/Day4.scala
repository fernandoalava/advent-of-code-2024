import scala.io.Source
import scala.util.Try

@main def day4(): Unit = {

  val word = "XMAS"

  def getInput: Array[Array[Char]] = {
    Source
      .fromFile("src/main/resources/day4/input.txt")
      .getLines()
      .map(_.toCharArray())
      .toArray
  }

  val inputTest = Iterator(
    "MMMSXXMASM",
    "MSAMXMSMSA",
    "AMXSXMAAMM",
    "MSAMASMSMX",
    "XMASAMXAMM",
    "XXAMMXXAMA",
    "SMSMSASXSS",
    "SAXAMASAAA",
    "MAMMMXMMMM",
    "MXMXAXMASX"
  ).map(line => line.toCharArray()).toArray

  def reviewDirectionsPluzzle1(
      row: Int,
      col: Int,
      input: Array[Array[Char]]
  ): Int = {
    // goUp row-1, , col
    // goDown row+1 , col
    // goLeft row , col-1
    // goRight row , col+1
    // goUpRight row-1, col+1
    // goUpLeft row-1,col-1
    // goDownRight row+1, col+1
    // goDownLeft row+1, col-1
    val goUp = (0 to 3)
      .map(i => Try(input(row - i)(col)).toOption)
      .filter(_.isDefined)
      .map(_.get)
      .mkString
    val goDown = (0 to 3)
      .map(i => Try(input(row + i)(col)).toOption)
      .filter(_.isDefined)
      .map(_.get)
      .mkString
    val goLeft = (0 to 3)
      .map(i => Try(input(row)(col - i)).toOption)
      .filter(_.isDefined)
      .map(_.get)
      .mkString
    val goRight = (0 to 3)
      .map(i => Try(input(row)(col + i)).toOption)
      .filter(_.isDefined)
      .map(_.get)
      .mkString
    val goUpRight = (0 to 3)
      .map(i => Try(input(row - i)(col + i)).toOption)
      .filter(_.isDefined)
      .map(_.get)
      .mkString
    val goUpLeft = (0 to 3)
      .map(i => Try(input(row - i)(col - i)).toOption)
      .filter(_.isDefined)
      .map(_.get)
      .mkString
    val goDownRight = (0 to 3)
      .map(i => Try(input(row + i)(col + i)).toOption)
      .filter(_.isDefined)
      .map(_.get)
      .mkString
    val goDownLeft = (0 to 3)
      .map(i => Try(input(row + i)(col - i)).toOption)
      .filter(_.isDefined)
      .map(_.get)
      .mkString
    val words = Seq(
      goUp,
      goDown,
      goLeft,
      goRight,
      goUpRight,
      goUpLeft,
      goDownRight,
      goDownLeft
    ).filterNot(_ == "").count(_ == word)
    return words
  }

  def reviewDirectionsPuzzle2(
      row: Int,
      col: Int,
      input: Array[Array[Char]]
  ): Boolean = {
    val goUpRight = Try(input(row - 1)(col + 1)).getOrElse('&')
    val goUpLeft = Try(input(row - 1)(col - 1)).getOrElse('&')
    val goDownRight = Try(input(row + 1)(col + 1)).getOrElse('&')
    val goDownLeft = Try(input(row + 1)(col - 1)).getOrElse('&')

    val current = Array(
      Array(goUpLeft, ' ', goUpRight).mkString,
      Array(' ', 'A', ' ').mkString,
      Array(goDownLeft, ' ', goDownRight).mkString
    ).mkString

    val valids = Array(
      Array("M S", " A ", "M S").mkString,
      Array("M M", " A ", "S S").mkString,
      Array("S M", " A ", "S M").mkString,
      Array("S S", " A ", "M M").mkString
    )

    valids.contains(current)
  }

  def puzzle1(input: Array[Array[Char]]): Integer = {
    (for {
      row <- 0 until input.length
      col <- 0 until input.head.length
    } yield {
      (input(row)(col), row, col)
    }).filter(_._1 == 'X')
      .map(v => reviewDirectionsPluzzle1(v._2, v._3, input))
      .sum
  }

  def puzzle2(input: Array[Array[Char]]): Integer = {
    (for {
      row <- 0 until input.length
      col <- 0 until input.head.length
    } yield {
      (input(row)(col), row, col)
    }).filter(_._1 == 'A')
      .map(v => reviewDirectionsPuzzle2(v._2, v._3, input))
      .count(_ == true)
  }

  println(puzzle1(getInput))
  println(puzzle1(inputTest))
  println(puzzle2(inputTest))
  println(puzzle2(getInput))
}
