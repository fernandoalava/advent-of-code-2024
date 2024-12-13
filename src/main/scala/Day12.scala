import scala.io.Source
import scala.collection.mutable

object Day12 {
  val inputTest = """RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
""".split("\n").toVector

  val inputTest2 = """OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
""".split("\n").toVector

  val inputTest3 = """AAAA
BBCD
BBCC
EEEC
""".split("\n").toVector

  type Mapa = Map[Coordinate, Char]
  case class Coordinate(row: Int, column: Int)

  var visited: mutable.Set[Coordinate] = mutable.Set.empty

  def getInput(): Vector[String] = {
    Source
      .fromFile("src/main/resources/day12/input.txt")
      .getLines()
      .toVector
  }

  def parseInput(input: Vector[String]): Mapa = {
    input
      .map(_.toCharArray().toVector)
      .zipWithIndex
      .flatMap((row, rowIndex) =>
        row.zipWithIndex.map((value, columnIndex) =>
          (new Coordinate(rowIndex, columnIndex), value)
        )
      )
      .toMap
  }

  val directions: Vector[Coordinate => Coordinate] = Vector(
    (coordinate: Coordinate) => coordinate.copy(row = coordinate.row + 1),
    (coordinate: Coordinate) => coordinate.copy(row = coordinate.row - 1),
    (coordinate: Coordinate) => coordinate.copy(column = coordinate.column + 1),
    (coordinate: Coordinate) => coordinate.copy(column = coordinate.column - 1)
  )

  def walk(
      mapa: Mapa,
      prevCoordinate: Coordinate,
      region: Vector[Coordinate],
      visited: mutable.Set[Coordinate]
  ): Vector[Vector[Coordinate]] = {
    val newRegion = region :+ prevCoordinate
    if (visited.contains(prevCoordinate)) {
      return Vector.empty
    }
    visited += prevCoordinate
    val neighbors = mapa
      .get(prevCoordinate)
      .map(prevType => {
        directions
          .map(_(prevCoordinate))
          .flatMap(maybeNeighborCoordinate =>
            mapa.get(maybeNeighborCoordinate) match
              case Some(sameTypeNeighbor) if sameTypeNeighbor == prevType =>
                Some(maybeNeighborCoordinate)
              case _ => None
          )
          .flatMap(neighborCoordinate =>
            walk(
              mapa,
              neighborCoordinate,
              newRegion,
              visited
            )
          )
      })
      .getOrElse(Vector.empty)
    Vector(newRegion) ++ neighbors
  }

  def collectRegions(mapa: Mapa): Vector[Vector[(Char, Coordinate)]] = {
    val visited: mutable.Set[Coordinate] = mutable.Set.empty
    mapa.keySet
      .map(coordinate => walk(mapa, coordinate, Vector.empty, visited))
      .toVector
      .map(_.flatten.toSet)
      .filter(_.nonEmpty)
      .map(_.toVector.map(coordinate => (mapa.get(coordinate).get, coordinate)))
  }

  def calculatePrice(region: Vector[(Char, Coordinate)], mapa: Mapa): Int = {
    val area = region.size
    val perimeter = region
      .flatMap(plant => {
        directions
          .map(_(plant._2))
          .map(mapa.get)
          .map(maybePlantType => {
            maybePlantType match
              case None                                     => 1
              case Some(plantType) if plantType != plant._1 => 1
              case _                                        => 0

          })
      })
      .sum
    return area * perimeter
  }

  def calculatePriceWithBulkDiscount(
      region: Vector[(Char, Coordinate)],
      mapa: Mapa
  ): Int = {
    val area = region.size
    val corners = region.foldLeft(0)((acc, value) => {

      val up =
        mapa.get(value._2.copy(row = value._2.row - 1))
      val right = mapa
        .get(value._2.copy(column = value._2.column + 1))

      val upRight = mapa
        .get(
          value._2.copy(row = value._2.row - 1, column = value._2.column + 1)
        )

      val left = mapa
        .get(value._2.copy(column = value._2.column - 1))

      val upLeft = mapa
        .get(
          value._2.copy(row = value._2.row - 1, column = value._2.column - 1)
        )

      val down =
        mapa.get(value._2.copy(row = value._2.row + 1))
      val downRight = mapa
        .get(
          value._2.copy(row = value._2.row + 1, column = value._2.column + 1)
        )

      val downLeft = mapa
        .get(
          value._2.copy(row = value._2.row + 1, column = value._2.column - 1)
        )

      var c = 0

      if (
        up != Some(value._1) && right != Some(value._1) && upRight != Some(
          value._1
        )
      ) {
        c += 1
      }

      if (
        up != Some(value._1) && right != Some(value._1) && upRight == Some(
          value._1
        )
      ) {
        c += 1
      }

      if (
        up == Some(value._1) && right == Some(value._1) && upRight != Some(
          value._1
        )
      ) {
        c += 1
      }

      if (
        up != Some(value._1) && left != Some(value._1) && upLeft != Some(
          value._1
        )
      ) {
        c += 1
      }

      if (
        up != Some(value._1) && left != Some(value._1) && upLeft == Some(
          value._1
        )
      ) {
        c += 1
      }

      if (
        up == Some(value._1) && left == Some(value._1) && upLeft != Some(
          value._1
        )
      ) {
        c += 1
      }

      if (
        down != Some(value._1) && left != Some(value._1) && downLeft != Some(
          value._1
        )
      ) {
        c += 1
      }

      if (
        down != Some(value._1) && left != Some(value._1) && downLeft == Some(
          value._1
        )
      ) {
        c += 1
      }

      if (
        down == Some(value._1) && left == Some(value._1) && downLeft != Some(
          value._1
        )
      ) {
        c += 1
      }

      if (
        down != Some(value._1) && right != Some(value._1) && downRight != Some(
          value._1
        )
      ) {
        c += 1
      }

      if (
        down == Some(value._1) && right == Some(value._1) && downRight != Some(
          value._1
        )
      ) {
        c += 1
      }

      if (
        down != Some(value._1) && right != Some(value._1) && downRight == Some(
          value._1
        )
      ) {
        c += 1
      }

      acc + c
    })
    return area * corners
  }

  def puzzle1(input: Vector[String]): Int = {
    val mapa = parseInput(input)
    val regions = collectRegions(mapa)
    regions.map(region => calculatePrice(region, mapa)).sum
  }

  def puzzle2(input: Vector[String]): Int = {
    val mapa = parseInput(input)
    val regions = collectRegions(mapa)
    regions.map(region => calculatePriceWithBulkDiscount(region, mapa)).sum
  }

}

@main def day12Main() = {
  import Day12._
  println(puzzle1(getInput()))
  println(puzzle2(getInput()))
}
