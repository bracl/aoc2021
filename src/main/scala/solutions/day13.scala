package solutions

import utils.IO._

object day13 extends App {

  val xxx = readFile(13)

  val dots = xxx
    .filter(_.nonEmpty)
    .filterNot(_.startsWith("fold"))
    .map(_.trim().split(','))
    .map(l => (l.head.toInt, l.last.toInt))
    .toSet
  val folds = xxx.filter(_.startsWith("fold"))

  def performFold(coords: Set[(Int, Int)], X: Boolean, value: Int): Set[(Int, Int)] =
    coords.map({
      case (x, y) =>
        if (X)
          if (x < value) (x, y) else (value - (x - value), y)
        else if (y < value) (x, y)
        else (x, value - (y - value))
    })

  val afterFold1 = performFold(dots, X = true, 655)
  println(s"Part 1: ${afterFold1.size}\n")

  val end = folds.foldLeft(dots)({
    case (dots, foldInstruction) =>
      performFold(dots, foldInstruction.contains("x"), foldInstruction.split("=").last.toInt)
  })

  val grid = (0 to end.maxBy(_._2)._2).toList.map(i => (0 to end.maxBy(_._1)._1).toList.map(_ => " "))

  val yyy = end.foldLeft(grid)({
    case (grid, (x, y)) =>
      updateMatrix(grid, x, y, "#")
  })

  println("Part 2")
  printGrid(yyy, "")

}
