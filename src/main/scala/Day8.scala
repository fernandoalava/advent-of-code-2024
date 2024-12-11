import scala.io.Source
object Day8 {

  sealed trait Element
  object Element {
    type Frequency = Char
    case object Free extends Element
    case class Antena(frequency: Frequency) extends Element
    def toElement(value: Char): Element = {
      value match
        case v: Char if v.isLetterOrDigit => new Element.Antena(v)
        case _                            => Element.Free
    }
    def isAntena(element: Element): Boolean = {
      element match
        case Free      => false
        case Antena(_) => true

    }
  }

  case class Point(row: Int, column: Int) {
    def -(that: Point): Point =
      new Point(this.row - that.row, this.column - that.column)
    def +(that: Point): Point =
      new Point(this.row + that.row, this.column + that.column)
    def *(scala: Int): Point = new Point(this.row * 2, this.column * 2)

  }
  case class Mapa(elements: Map[Element, Set[Point]], rows: Int, columns: Int) {
    def isInBounds(point: Point): Boolean = {
      point.row >= 0 && point.row < rows && point.column >= 0 && point.column < columns
    }
  }

  def parseInput(input: Seq[String]): Mapa = {
    new Mapa(
      input.zipWithIndex
        .flatMap((line, row) =>
          line
            .toCharArray()
            .zipWithIndex
            .map((value, column) => {
              (new Point(row, column), Element.toElement(value))
            })
        )
        .groupMap(_._2)(_._1)
        .map(value => (value._1, value._2.toSet)),
      input.length,
      input.head.length()
    )
  }

  val testInput = """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............""".split("\n")

  def getInput(): Seq[String] = {
    Source.fromFile("src/main/resources/day8/input.txt").getLines().toSeq
  }

  def combinePoints(points: Set[Point]): Seq[(Point, Point)] = {
    points.toSeq
      .combinations(2)
      .flatMap { case Seq(a, b) =>
        Seq((a, b), (b, a))
      }
      .toSeq
  }

  def puzzle1(mapa: Mapa): Int = {
    mapa.elements
      .filter(v => Element.isAntena(v._1))
      .map { case (element, points) =>
        element -> combinePoints(points)
      }
      .flatMap {
        { case (_, points) =>
          points.map { case (pointA, pointB) =>
            val distance = pointB - pointA
            val relativeDistance = distance * 2
            val antiNodeLocation = pointA + relativeDistance
            if (mapa.isInBounds(antiNodeLocation)) {
              Some(antiNodeLocation)
            } else {
              None
            }
          }

        }
      }
      .toSet
      .count(_.isDefined)
  }

  def puzzle2(mapa: Mapa): Int = {
    mapa.elements
      .filter(v => Element.isAntena(v._1))
      .map { case (element, points) =>
        element -> combinePoints(points)
      }
      .flatMap {
        { case (_, points) =>
          points.flatMap { case (pointA, pointB) =>
            val distance = pointB - pointA
            val relativeDistance = distance
            val positiveDirectionAntiNodes = Iterator
              .iterate(pointA)(_ + relativeDistance)
              .takeWhile(mapa.isInBounds(_))
              .toSeq
            val negativeDirectionAntiNodes = Iterator
              .iterate(pointA)(_ - relativeDistance)
              .takeWhile(mapa.isInBounds(_))
              .toSeq
            positiveDirectionAntiNodes ++ negativeDirectionAntiNodes
          }
        }
      }
      .toSet
      .size
  }

  @main def main(): Unit = {
    println(puzzle1(parseInput(testInput.toIndexedSeq)))
    println(puzzle2(parseInput(testInput.toIndexedSeq)))
    println(puzzle1(parseInput(getInput())))
    println(puzzle2(parseInput(getInput())))
  }
}
