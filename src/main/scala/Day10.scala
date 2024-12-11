import scala.io.Source

object Day10 {
  val testInput = """
  89010123
  78121874
  87430965
  96549874
  45678903
  32019012
  01329801
  10456732""".split("\n").toVector

  type Mapa = Map[Coordinate, Int]
  case class Coordinate(row: Int, column: Int)

  val trailHeadHeight = 0
  val trailTailHeight = 9

  val directions: Vector[Coordinate => Coordinate] = Vector(
    (coordinate: Coordinate) => coordinate.copy(row = coordinate.row + 1),
    (coordinate: Coordinate) => coordinate.copy(row = coordinate.row - 1),
    (coordinate: Coordinate) => coordinate.copy(column = coordinate.column + 1),
    (coordinate: Coordinate) => coordinate.copy(column = coordinate.column - 1)
  )

  def getInput(): Vector[String] = {
    Source
      .fromFile("src/main/resources/day10/input.txt")
      .getLines()
      .toVector
  }

  def parseInput(input: Vector[String]): Mapa =
    input
      .map(_.toCharArray().map(_.asDigit).toVector)
      .zipWithIndex
      .flatMap((row, rowIndex) =>
        row.zipWithIndex.map((value, columnIndex) =>
          (new Coordinate(rowIndex, columnIndex), value)
        )
      )
      .toMap

  def findTailHeads(mapa: Mapa): Iterable[Coordinate] = {
    mapa.filter(_._2 == trailHeadHeight).map(_._1)
  }

  def collect(
      mapa: Mapa,
      previousCoordinate: Coordinate,
      path: Vector[Coordinate]
  ): Vector[Vector[Coordinate]] = {
    val newPath = path :+ previousCoordinate
    val previosHeight = mapa.get(previousCoordinate).get
    val validTrail =
      if (previosHeight == trailTailHeight) Vector(newPath) else Vector.empty

    val childrenTrails = directions
      .map(_(previousCoordinate))
      .flatMap(coordinate =>
        mapa.get(coordinate) match
          case Some(height) if ((height - previosHeight) == 1) =>
            Some(coordinate)
          case _ => None
      )
      .flatMap(nextCoordinate => collect(mapa, nextCoordinate, newPath))
    validTrail ++ childrenTrails
  }

  def puzzle1(input: Vector[String]): Int = {
    val mapa = parseInput(input)
    val trailHeads = findTailHeads(mapa)
    val scores = trailHeads.map(tailCoordinate =>
      collect(mapa, tailCoordinate, Vector.empty)
        .groupMap(v => v.last)(identity)
        .map(_._2.head)
        .size
    )
    scores.sum
  }

  def puzzle2(input: Vector[String]): Int = {
    val mapa = parseInput(input)
    val trailHeads = findTailHeads(mapa)
    val ratings = trailHeads.map(tailCoordinate =>
      collect(mapa, tailCoordinate, Vector.empty).size
    )
    ratings.sum
  }

}
@main def mainDay10() = {
  println(Day10.puzzle1(Day10.testInput));
  println(Day10.puzzle2(Day10.testInput));
  println(Day10.puzzle1(Day10.getInput()));
  println(Day10.puzzle2(Day10.getInput()));
}
