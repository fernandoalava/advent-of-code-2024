import scala.io.Source
import scala.annotation.tailrec

@main def day9(): Unit = {

  def getInput(): String = {
    Source
      .fromFile("src/main/resources/day9/input.txt")
      .getLines()
      .mkString
  }

  val testInput = """2333133121414131402"""

  sealed trait Block
  case object Free extends Block
  case class File(id: Int) extends Block

  def generateLayout(input: String): Vector[Block] = {
    input
      .map(_.asDigit)
      .sliding(2, 2)
      .map {
        case Seq(a, b) => Right((a, b))
        case Seq(a)    => Left(a)
      }
      .zipWithIndex
      .flatMap {
        case (Right(file, freeSpace), index) => {
          Seq.fill(file)(new File(index)) ++ Seq.fill(freeSpace)(Free)
        }
        case (Left(file), index) => {
          Seq.fill(file)(new File(index))
        }
      }
      .toVector
  }

  @tailrec
  def removeGaps(seq: Vector[Block], i: Int, j: Int): Vector[Block] = {
    if (i >= j) seq
    else {
      val next = (seq(i), seq(j)) match
        case (File(_), _) => (seq, i + 1, j)
        case (_, Free)    => (seq, i, j - 1)
        case (a @ Free, b @ File(_)) => {
          val newSeq = seq.updated(i, b).updated(j, a)
          (newSeq, i + 1, j - 1)
        }

      removeGaps(next._1, next._2, next._3)
    }
  }

  def calculateFileSystemCheckSum(seq: Vector[Block]): Long = {
    seq.zipWithIndex.flatMap {
      case (value, index) => {
        value match
          case Free => {
            None
          }
          case File(id) => {
            Some((index.toLong * id).toLong)
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

  println(puzzle1(testInput))
  println(puzzle1(getInput()))

}
