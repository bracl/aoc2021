package solutions

import utils.IO.readFile

import scala.annotation.tailrec

object day11 extends App {

  type Octopuses = Vector[Vector[Int]]

  val startingOctopuses: Octopuses = readFile(11).map(_.split("").toList.map(_.toInt).toVector).toVector

//  val startingOctopuses: Octopuses = readFile("/Users/bradleyking/dev/aoc2021/src/main/scala/input/11.b.txt")
//    .map(_.split("").toList.map(_.toInt).toVector)
//    .toVector

  val flashes = 0
  val rows = startingOctopuses.length
  val columns = startingOctopuses.head.length

  def chargedOct(octs: Octopuses) =
    for {
      i <- 0 until rows
      j <- 0 until columns
      if octs(j)(i) > 9
    } yield (i, j)

  def explodeCoordinates(coords: List[(Int, Int)], hasAlreadyFlashed: Set[(Int, Int)]): List[(Int, Int)] = {
    val h = List(0, 1, 1, 1, 0, -1, -1, -1)
    val v = List(-1, -1, 0, 1, 1, 1, 0, -1)
    val zipped: List[(Int, Int)] = h.zip(v)
    val exploded = coords.flatMap { c =>
      zipped.map({ case (i, j) => (c._1 + i, c._2 + j) })
    }
    exploded
      .filter({
        case (i, j) =>
          i >= 0 &&
            j >= 0 &&
            j < rows &&
            i < columns
      })
      .filterNot({
        case (i, j) =>
          hasAlreadyFlashed.contains((i, j))
      })
  }

  def flashDay(octopuses: Octopuses): (Octopuses, Int) = {

    def updateMatrix(mat: Vector[Vector[Int]], i: Int, j: Int, value: Int) =
      mat.updated(j, mat(j).updated(i, value))

    def applyFlashes(octs: Octopuses, coords: List[(Int, Int)]): Octopuses =
      coords.foldLeft(octs)({
        case (o, (i, j)) =>
          val temp = updateMatrix(o, i, j, o(j)(i) + 1)
//          println(i, j)
//          printOcts(temp)
          temp
      })

    def zeroChargedOctopuses(octopuses: Octopuses, coords: List[(Int, Int)]) =
      coords.foldLeft(octopuses)({
        case (o, (i, j)) => updateMatrix(o, i, j, 0)
      })

    @tailrec
    def flashNines(octs: Octopuses, totalFlashes: Int = 0, flashedToday: Set[(Int, Int)] = Set()): (Octopuses, Int) = {
      val chargedOctCoords = chargedOct(octs).toList
      val willHaveFlashed: Set[(Int, Int)] = chargedOctCoords.foldLeft(flashedToday)({
        case (ft, c) => ft + c
      })
      chargedOctCoords.length match {
        case 0 => (octs, totalFlashes)
        case _ =>
          val surrounding = explodeCoordinates(chargedOctCoords, willHaveFlashed)
          val newOcts: Octopuses = applyFlashes(octs, surrounding)
          val zerod = zeroChargedOctopuses(newOcts, chargedOctCoords)
          flashNines(zerod, totalFlashes + chargedOctCoords.length, willHaveFlashed)
      }
    }

    flashNines(octopuses)
  }

  def printOcts(o: Octopuses): Unit = println(o.map(v => v.mkString(",")).mkString("\n") + "\n")

  (1 to 500).foldLeft(
    (startingOctopuses, flashes)
  ) {
    case ((dayStartOctopuses, dayStartFlashes), day) =>
      println(s"Starting day $day")

      val increased = dayStartOctopuses.map(row => row.map(_ + 1))
      val (flashed, theDaysFlashes) = flashDay(increased)
      val newFlashed = dayStartFlashes + theDaysFlashes

      if (day == 100) println(s"Flashes After 100 Days: $newFlashed")
      if (theDaysFlashes == rows * columns) {
        println(s"Takes $day to have all Octopuses flash")
        throw new Exception("Hush child, you've done all you need to...")
      }
      (flashed, newFlashed)
  }

}
