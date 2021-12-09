package solutions

import utils.IO.readFile

object day7 extends App {

  val crabPositions = readFile(7).head.split(',').map(_.toInt).toList

  println(crabPositions)

  def accurateFuelCost(from: Int, to: Int): Int = {
    val abs = math.abs(from - to)
    (abs * (abs + 1)) / 2
  }

  def minFuelCost(positions: List[Int]): Int =
    (1 to positions.max).map(i => (i, positions.map(cp => math.abs(cp - i)).sum)).minBy(_._2)._2

  def minAccurateFuelCost(positions: List[Int]): Int =
    (1 to positions.max).map(i => (i, positions.map(cp => accurateFuelCost(cp, i)).sum)).minBy(_._2)._2

  val part1 = minFuelCost(crabPositions)
  val part2 = minAccurateFuelCost(crabPositions)

  println(part1)
  println(part2)
}
