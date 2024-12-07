import scala.io.Source

@main def day7(): Unit = {

  sealed trait Operation {
    def calculate(x: Long, y: Long): Long
  }

  object Operation {
    case object Add extends Operation {
      def calculate(x: Long, y: Long): Long = x + y
    }

    case object Mul extends Operation {
      def calculate(x: Long, y: Long): Long = x * y
    }

    case object Concat extends Operation {
      def calculate(x: Long, y: Long): Long = s"$x$y".toLong
    }
  }

  def parseInput(input: Seq[String]): Seq[(Long, Seq[Long])] = {
    input.map(line =>
      line.trim.split(":") match
        case Array(target, values) =>
          (target.toLong, values.trim.split(" ").map(_.toLong))
    )
  }

  def getInput(): Seq[String] = {
    Source.fromFile("src/main/resources/day7/input.txt").getLines().toSeq
  }

  def generateCombinations(
      operations: Seq[Operation],
      numSlots: Int
  ): Seq[Seq[Operation]] = {
    (numSlots match {
      case 0 =>
        Seq(
          Seq()
        )
      case _ =>
        for {
          operation <- operations
          combination <- generateCombinations(operations, numSlots - 1)
        } yield Seq(operation) ++ combination
    })
  }

  def isValid(
      target: Long,
      operands: Seq[Long],
      supportedOperations: Set[Operation]
  ): Boolean = {
    val slots = operands.length - 1
    val combinations =
      generateCombinations(supportedOperations.toSeq, slots)
    combinations
      .find(combination => {
        (0 until operands.size - 1)
          .foldLeft(operands(0))((accumulator, index) =>
            combination(index).calculate(accumulator, operands(index + 1))
          ) == target
      })
      .fold(false)(_ => true)
  }

  val testInput = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""

  def puzzle1(input: Seq[String]): Long = {
    parseInput(input)
      .map(a => (a._1, isValid(a._1, a._2, Set(Operation.Add, Operation.Mul))))
      .filter(_._2 == true)
      .map(_._1)
      .sum
  }

  /** It should be improve current approach is using brute force and take 4s to
    * process in my machine, possible improvement seems to be recursion
    */
  def puzzle2(input: Seq[String]): Long = {
    parseInput(input)
      .map(a =>
        (
          a._1,
          isValid(
            a._1,
            a._2,
            Set(Operation.Add, Operation.Mul, Operation.Concat)
          )
        )
      )
      .filter(_._2 == true)
      .map(_._1)
      .sum
  }

  println(puzzle1(testInput.split("\n")))
  println(puzzle2(testInput.split("\n")))

  println(puzzle1(getInput()))
  println(Utils.withExecutionTime(puzzle2(getInput())))
}
