@main def day9(): Unit = {
  val testInput = """2333133121414131402"""

  def generateLayout(input: String): String = {
    input
      .split("")
      .sliding(2, 2)
      .map {
        case Array(a, b) => Right((a.toInt, b.toInt))
        case Array(a)    => Left(a.toInt)
      }
      .toSeq
      .zipWithIndex
      .map {
        case (Right(file, freeSpace), index) =>
          s"${index.toString * file}${"." * freeSpace}"
        case (Left(file), index) => s"${index.toString * file}"
      }
      .mkString
  }

  println(generateLayout(testInput))

}
