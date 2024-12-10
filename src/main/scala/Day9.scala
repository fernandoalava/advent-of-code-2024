import scala.io.Source
import scala.annotation.tailrec
import scala.collection.immutable.Queue

@main def day9(): Unit = {

  def getInput(): String = {
    Source
      .fromFile("src/main/resources/day9/input.txt")
      .getLines()
      .mkString
  }

  val testInput = """2333133121414131402"""

  sealed trait Block
  object Block {
    def isFile(block: Block): Boolean = {
      block match
        case Free    => false
        case File(_) => true
    }
    def isFree(block: Block): Boolean = {
      block match
        case Free    => true
        case File(_) => false
    }
  }
  case object Free extends Block {
    override def toString(): String = "."
  }
  case class File(id: Int) extends Block {
    override def toString(): String = s"${id}"
  }

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

  def findFreeBlocks(
      leftMost: Vector[Block],
      until: Int,
      size: Int
  ): Option[Vector[Int]] = {
    leftMost.zipWithIndex
      .sliding(size)
      .find(window => window.map(_._1).forall(Block.isFree))
      .map(_.map(_._2))
  }

  @tailrec
  def fragment(
      input: Vector[Block],
      lastFileBlock: File
  ): Vector[Block] = {
    if (lastFileBlock.id == 0) {
      input
    } else {
      val files = input.zipWithIndex.filter { a =>
        a._1 match
          case File(lastFileBlock.id) => true
          case _                      => false

      }
      val nextFileBlock = lastFileBlock.copy(lastFileBlock.id - 1)
      val leftmost = input.slice(0, files.head._2)
      val nextInput =
        findFreeBlocks(leftmost, files.head._2, files.length).fold(input)(
          freeBlock =>
            files.zipWithIndex.foldLeft(input)((prevInput, value) => {
              val (file, index) = value
              prevInput
                .updated(freeBlock(index), file._1)
                .updated(file._2, Free)
            })
        )
      fragment(nextInput, nextFileBlock)
    }
  }

  def puzzle1(input: String): Long = {
    val layout = generateLayout(input)
    val withOutGaps =
      removeGaps(layout, 0, layout.length - 1)
    calculateFileSystemCheckSum(withOutGaps)
  }

  def puzzle2(input: String): Long = {
    val layout = generateLayout(input)
    val lastFileBlock = layout.findLast(Block.isFile).get.asInstanceOf[File]
    val fragmented = fragment(layout, lastFileBlock)
    calculateFileSystemCheckSum(fragmented)
  }

  println(puzzle2(testInput))
  println(puzzle2(getInput()))

}
