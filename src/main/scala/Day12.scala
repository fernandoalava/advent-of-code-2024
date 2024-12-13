import scala.io.Source
object Day12 {
  val inputTest = """
RRRRIICCFF
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

  type Mapa = Map[Coordinate, Char]
  case class Coordinate(row: Int, column: Int)

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


  def walk(mapa: Mapa, previousCoordinate: Coordinate) = {
    
  }


  def collectRegions(mapa:Mapa):Vector[(Char,Vector[Coordinate])] = {
    ???
  }

  def puzzle1(input: Vector[String]): Unit = {
    val mapa = parseInput(input)
    val regions = collectRegions(mapa)
    println(mapa)
    println(regions)
  }

}

@main def day12Main() = {
  import Day12._
  println(puzzle1(inputTest))
//   println(puzzle1(getInput()))
}
