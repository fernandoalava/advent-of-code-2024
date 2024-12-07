import scala.io.Source

sealed trait Element
case object Obstacle extends Element
case object Guard extends Element
case object Nothing extends Element

enum Direction:
  case Up
  case Down
  case Right
  case Left

object Element {
  def isGuard(element: Element): Boolean = element match
    case Guard => true
    case _     => false

}

case class Point(row: Int, column: Int)
case class Mapa(points: Map[Point, Element])

object Mapa {
  import Element.isGuard
  def getGuardInitialPosition(mapa: Mapa): Point = {
    mapa.points.find(point => isGuard(point._2)).get._1
  }
}

def getInput(): Seq[String] = {
  Source.fromFile("src/main/resources/day6/input.txt").getLines().toSeq
}

def parseInput(input: Seq[String]): Mapa = {

  val points = input.zipWithIndex.flatMap { case (line, row) =>
    line.split("").zipWithIndex.map {
      case (data, column) => {
        val element = data match {
          case "#" => Obstacle
          case "^" => Guard
          case _   => Nothing
        }
        (
          new Point(row, column),
          element
        )
      }
    }
  }.toMap
  new Mapa(points)
}

def getNextDirection(direction: Direction): Direction = {
  direction match
    case Direction.Up    => Direction.Right
    case Direction.Down  => Direction.Left
    case Direction.Right => Direction.Down
    case Direction.Left  => Direction.Up

}

def getNextPoint(
    currentPoint: Point,
    direction: Direction,
    mapa: Mapa
): Option[(Point, Element, Direction)] = {
  val possibleNextPoint = direction match
    case Direction.Up    => new Point(currentPoint.row - 1, currentPoint.column)
    case Direction.Down  => new Point(currentPoint.row + 1, currentPoint.column)
    case Direction.Right => new Point(currentPoint.row, currentPoint.column + 1)
    case Direction.Left =>
      new Point(currentPoint.row, currentPoint.column - 1)

  mapa.points.get(possibleNextPoint) match

    case Some(value) =>
      value match
        case Obstacle =>
          getNextPoint(currentPoint, getNextDirection(direction), mapa)
        case Guard   => Some(possibleNextPoint, value, direction)
        case Nothing => Some(possibleNextPoint, value, direction)
    case None => {
      None
    }
}

def walk(
    point: Point,
    positions: Set[(Point, Direction)],
    direction: Direction,
    mapa: Mapa
): Set[(Point, Direction)] = {
  val maybeNextPoint = getNextPoint(point, direction, mapa)
  val nextPositions = positions + ((point, direction))
  maybeNextPoint match
    case None        => nextPositions
    case Some(value) => walk(value._1, nextPositions, value._3, mapa)
}

def isWalkALoop(
    point: Point,
    positions: Set[(Point, Direction)],
    direction: Direction,
    mapa: Mapa
): Boolean = {
  val maybeNextPoint = getNextPoint(point, direction, mapa)
  val nextPositions = positions + ((point, direction))
  maybeNextPoint match
    case None => false
    case Some(value) => {
      if (nextPositions.contains((value._1, value._3))) {
        true
      } else {
        isWalkALoop(value._1, nextPositions, value._3, mapa)
      }

    }
}

def puzzle1(input: Seq[String]): Int = {
  val mapa = parseInput(input)
  val initialPosition = Mapa.getGuardInitialPosition(mapa)
  val visitedPoints =
    walk(
      initialPosition,
      Set((initialPosition, Direction.Up)),
      Direction.Up,
      mapa
    )
  visitedPoints.map(_._1).size
}

def puzzle2(input: Seq[String]): Int = {
  val mapa = parseInput(input)
  val initialPosition = Mapa.getGuardInitialPosition(mapa)
  val initialDirection = Direction.Up
  val visitedPoints =
    walk(
      initialPosition,
      Set((initialPosition, initialDirection)),
      Direction.Up,
      mapa
    )

  visitedPoints
    .map(_._1)
    .filterNot(_ == initialPosition)
    .map { point =>
      val newMapa = mapa.copy(points = mapa.points.updated(point, Obstacle))
      val isALoop = isWalkALoop(
        initialPosition,
        Set((initialPosition, Direction.Up)),
        Direction.Up,
        newMapa
      )
      (point, isALoop)
    }
    .filter(_._2)
    .size
}

val testInput =
  """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...""".split("\n")

@main def day6(): Unit = {
  println(puzzle1(testInput))
  println(puzzle2(testInput))
  println(puzzle1(getInput()))
  println(Utils.withExecutionTime(puzzle2(getInput())))
}
