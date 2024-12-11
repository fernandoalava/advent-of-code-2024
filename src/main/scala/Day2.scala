import scala.io.Source

@main def day2(): Unit = {

  def getInput: List[Array[Integer]] = {
    Source
      .fromFile("src/main/resources/day2/input.txt")
      .getLines()
      .map(line => line.split(" ").map(Integer.valueOf))
      .toList
  }

  def isSafe(report: Seq[Integer], withTolerance: Boolean = false): Boolean =
    Seq(report, report.reverse).exists { r =>
      val patched =
        if (withTolerance) (0 to r.length).map(r.patch(_, Seq.empty, 1))
        else Seq.empty
      (r +: patched).exists(_.sliding(2).forall { case Seq(a, b) =>
        a < b && b - a <= 3
      })
    }


  def puzzle1: Integer = {
    getInput
      .map(v => isSafe(v.toSeq, false))
      .count(_ == true)
  }

  def puzzle2: Integer = {
    getInput
      .map(v => isSafe(v.toSeq, true))
      .count(_ == true)
  }

  println(puzzle1)
  println(puzzle2)
}
