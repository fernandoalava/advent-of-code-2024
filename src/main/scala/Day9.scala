import scala.io.Source
import scala.annotation.tailrec

@main def day9(): Unit = {

  def getInput(): String = {
    Source
      .fromFile("src/main/resources/day9/input.txt")
      .getLines()
      .toList
      .mkString
  }

  val testInput = """2333133121414131402"""

  def generateLayout(input: String): Vector[Char] = {
    input
      .map(_.asDigit)
      .sliding(2, 2)
      .map {
        case Seq(a, b) => Right((a, b))
        case Seq(a)    => Left(a)
      }
      .zipWithIndex
      .flatMap {
        case (Right(file, freeSpace), index) =>
          s"${index.toString * file}${"." * freeSpace}"
        case (Left(file), index) => s"${index.toString * file}"
      }
      .toVector
  }

  @tailrec
  def removeGaps(seq: Vector[Char], i: Int, j: Int): Vector[Char] = {
    if (i >= j) seq
    else {
      val next = (seq(i), seq(j)) match
        case ('.', b) if b.isDigit => {
          val newSeq = seq.updated(i, b).updated(j, '.')
          (newSeq, i + 1, j - 1)
        }
        case ('.', '.')          => (seq, i, j - 1)
        case (a, _) if a.isDigit => (seq, i + 1, j)
        case (_, '.')            => (seq, i, j - 1)
        case _ => (seq, i + 1, j - 1)

      removeGaps(next._1, next._2, next._3)
    }
  }

  def calculateFileSystemCheckSum(seq: Vector[Char]): Long = {
    seq.zipWithIndex.flatMap {
      case (value, index) => {
        value match
          case v if v.isDigit => {
            Some((index * v.asDigit).toLong)
          }
          case _ => {
            None
          }
      }
    }.sum
  }

  def puzzle1(input: String): Long = {
    val layout = generateLayout(input)
    val withOutGaps =
      removeGaps(layout, 0, layout.length - 1)
    calculateFileSystemCheckSum(withOutGaps)
  }

  // println(puzzle1(testInput))
  // println(getInput())
  println(puzzle1(getInput()))

}
