import scala.io.Source

@main def day1(): Unit = {

  def getLists: (List[Integer], List[Integer]) = {
    Source
      .fromFile("src/main/resources/day1/input.txt")
      .getLines()
      .map(line =>
        line.split(" ") match
          case Array(pair1, _, _, pair2) =>
            (Integer.valueOf(pair1), Integer.valueOf(pair2))
      )
      .toList
      .unzip
  }

  def puzzle1(): Integer = {
    getLists match
      case Tuple2(list1, list2) =>
        list1.sorted
          .zip(list2.sorted)
          .map(r =>
            r match
              case Tuple2(_1, _2) =>
                Math.abs(_1 - _2)
          )
          .fold(0)(_ + _)
  }

  def puzzle2(): Integer = {
    val map = getLists._2.groupMapReduce(identity)(_ => 1)(_ + _)
    getLists._1.map(value => value * map.getOrElse(value, 0)).fold(0)(_ + _)
  }
  println(puzzle1())
  println(puzzle2())
}
