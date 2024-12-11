import scala.io.Source
import scala.util.Try

case class Input(rules: Map[Int, Seq[Int]], updates: Seq[Seq[Int]])

def parseInput(inputLines: Seq[String]): Input = {
  val lines = inputLines
    .map(line => {
      val maybeRule: Option[(Int, Int)] =
        line.split('|').filter(_.nonEmpty) match {
          case Array(x, y) => Some((x.toInt, y.toInt))
          case _           => None
        }
      val maybeUpdate: Option[Seq[Int]] =
        Try(line.split(",").map(_.toInt).toSeq).toOption
      if (maybeRule.isDefined) {
        maybeRule
      } else if (maybeUpdate.isDefined) {
        maybeUpdate
      } else {
        None
      }
    })
    .toSeq
    .filter(_.isDefined)

  val u = lines.collect {
    case Some(value) if value.isInstanceOf[Seq[Int]] =>
      value.asInstanceOf[Seq[Int]]
  }
  val r: Map[Int, Seq[Int]] = lines
    .collect {
      case Some(value) if value.isInstanceOf[Tuple2[Int, Int]] =>
        value.asInstanceOf[Tuple2[Int, Int]]
    }
    .groupMap(_._1) { case (_, values) =>
      values
    }
  return new Input(r, u)
}

def getInput(): Input = {
  parseInput(
    Source
      .fromFile("src/main/resources/day5/input.txt")
      .getLines()
      .toSeq
  )
}

def getMiddle(seq: Seq[Int]): Seq[Int] = {
  if (seq.isEmpty) Seq.empty
  else if (seq.length % 2 == 1) Seq(seq(seq.length / 2))
  else seq.slice((seq.length / 2) - 1, (seq.length / 2) + 1)
}

def predicate(rules: Map[Int, Seq[Int]])(a: Int, b: Int): Boolean = {
  val x = rules.get(a) match
    case None        => true
    case Some(value) => value.contains(b)

  val y = rules.get(b) match
    case None        => true
    case Some(value) => !value.contains(a)

  x && y
}

def isOrder(rules: Map[Int, Seq[Int]])(s: Seq[Int]): Boolean = {
  s.sliding(2).forall {
    case Seq(a, b) => predicate(rules)(a, b)
    case _         => true
  }
}

def puzzle1(input: Input): Int = {
  input.updates.filter(isOrder(input.rules)).flatMap(getMiddle).sum
}

def puzzle2(input: Input): Int = {
  input.updates
    .filter(a => !isOrder(input.rules)(a))
    .map(_.sortWith(predicate(input.rules)))
    .flatMap(getMiddle)
    .sum
}

val testInput = """
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"""

@main def day5(): Unit = {
  val input = parseInput(testInput.split("\n").toIndexedSeq)
  println(puzzle1(input))
  println(puzzle2(input))
  println(puzzle1(getInput()))
  println(puzzle2(getInput()))
}
