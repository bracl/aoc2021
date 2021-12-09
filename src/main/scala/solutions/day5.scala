package solutions

import utils.IO.readFile

object day5 extends App {

  type Line = ((Int, Int), (Int, Int))
  type Coord = (Int, Int)

  val lines = readFile(5)

  def splitLine(line: List[Coord]): Line = {
    val start = line(0)
    val end = line(1)
    start -> end
  }

  def parseLine(s: String): List[Coord] = {
    val parts: List[String] = s.split(" -> ").toList
    parts.flatMap(s =>
      s.split(", ").toList.map { s =>
        val startEnd = s.split(",")
        (startEnd(0).toInt, startEnd(1).toInt)
      }
    )
  }

  def filterHorizontalVertical(c: Line): Boolean = {
    val (start, end) = (c._1, c._2)

    start._1 == end._1 || start._2 == end._2
  }

  def addCoords(a: Coord, b: Coord, howManyTimes: Int): Coord =
    (a._1 + b._1 * howManyTimes, a._2 + b._2 * howManyTimes)

  def allCoordsOnDiagonalLine(line: Line): List[Coord] = {
    val (start, end) = (line._1, line._2)

    val direction =
      if (start._1 < end._1 && start._2 < end._2) (1, 1)
      else if (start._1 < end._1 && start._2 > end._2) (1, -1)
      else if (start._1 > end._1 && start._2 < end._2) (-1, 1)
      else (-1, -1)

    val step = math.abs(start._1 - end._1)
    val step2 = math.abs(start._2 - end._2)
    assert(step == step2)

    val diagonal = for {
      i <- 0 to step
    } yield addCoords(start, direction, i)
    diagonal.toList
  }

  def allCoordsOnStraightLine(line: Line): List[Coord] = {
    val (start, end) = (line._1, line._2)

    val horizontalDirection = if (start._1 <= end._1) 1 else -1
    val verticalDirection = if (start._2 <= end._2) 1 else -1

    val a = for {
      i <- start._1 to end._1 by horizontalDirection
      j <- start._2 to end._2 by verticalDirection
    } yield (i, j)
    a.toList
  }

  def allCoordsOnLine(line: Line): List[Coord] =
    if (filterHorizontalVertical(line)) allCoordsOnStraightLine(line)
    else allCoordsOnDiagonalLine(line)

  val allLines: List[Line] = lines.map(parseLine).map(splitLine)
  val straightLines: List[Line] = allLines.filter(filterHorizontalVertical)

  println(s"Straight Lines: ${straightLines.length}")
  println(s"Diagonal Lines: ${lines.length - straightLines.length}\n")

  val straightCoords: List[Coord] = straightLines.flatMap(allCoordsOnStraightLine)
  val diagonalCoords: List[Coord] = allLines.flatMap(allCoordsOnLine)

  val part1TwoLinesOverlap = straightCoords.groupBy(identity).count(_._2.length >= 2)
  val part2TwoLinesOverlap = diagonalCoords.groupBy(identity).count(_._2.length >= 2)

  println(s"part1: $part1TwoLinesOverlap")
  println(s"part2: $part2TwoLinesOverlap")

  val brad = "confused"
}
