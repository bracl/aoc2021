package solutions

import utils.IO._

import scala.annotation.tailrec
import scala.collection.mutable

object day15 extends App {

  val xxx = readFile(15).map(_.trim().split("").toVector.map(_.toInt)).toVector

//  printGrid(xxx)

  val endy = xxx.length - 1
  val endx = xxx.head.length - 1

  println(endx)
  println(endy)

  def availableMoves(value: List[(Int, Int)], failed: Set[(Int, Int)]) = {
    val current = value.last
    List((-1, 0), (1, 0), (0, 1), (0, -1))
      .map(m => (current._1 + m._1, current._2 + m._2))
      .filter(c =>
        c._1 >= 0 &&
          c._1 <= endx &&
          c._2 >= 0 &&
          c._2 <= endy
      )
      .filterNot(c => value.contains(c))
      .filterNot(c => failed.contains(c))
  }

  var states = scala.collection.mutable.Map[(Int, Int), Int]()

  def costToZeroZero(
    grid: Vector[Vector[Int]],
    x: Int,
    y: Int
  ): Int = {
    if (states.keySet.contains((x, y)))
      return states((x, y))
    if (x < 0 || y < 0 || x > endx || y > endy)
      return 999999
    if (x == endx && y == endy)
      return grid(y)(x)
    val ans: Int =
      grid(y)(x) + List(
        costToZeroZero(grid, x + 1, y),
        costToZeroZero(grid, x, y + 1)
      ).min
    states + ((x, y) -> ans)
    ans
  }
  println(costToZeroZero(xxx, 0, 0))

//  def playGame(grid: Vector[Vector[Int]]) = {
////    def loop(grid: Vector[Vector[Int]], path: List[(Int, Int)], allPaths: Set[List[(Int, Int)]]) =
////      path.last match {
////        case (x, y) if x == endx && y == endy => path
////        case (x, y) =>
////          val am = availableMoves(path)
////          val newPaths = am.map(c => path :+ c).toSet
////          newPaths.foldLeft(allPaths)({
////            case (ap, p) => loop(grid, p, ap + newPaths)
////          })
////      }
//
////    @tailrec
////    def moveLocalMinimum(
////      grid: Vector[Vector[Int]],
////      path: List[(Int, Int)],
////      failedCoords: Set[(Int, Int)] = Set()
////    ): List[(Int, Int)] = {
////      val current = path.last
////      current match {
////        case (x, y) if x == endx && y == endy => path
////        case (x, y) =>
////          val am = availableMoves(path, failedCoords)
////          am match {
////            case Nil => moveLocalMinimum(grid, path.dropRight(1), failedCoords + current)
////            case xx =>
////              val minNextMove: (Int, Int) = am.minBy(c => grid(c._2)(c._1))
////              moveLocalMinimum(grid, path :+ minNextMove, failedCoords)
////          }
////      }
////    }
//
//    @tailrec
//    def moveBackwards(grid: Vector[Vector[Int]], path: List[(Int, Int)]): List[(Int, Int)] = {
//      val current = path.last
//      current match {
//        case (x, y) if x == 0 && y == 0 => path
//        case (x, y) =>
//          val am = availableMoves(path, Set())
//          assert(am.nonEmpty)
//          val minCostMove = am.minBy(c => costToZeroZero(grid, c._1, c._2))
//          moveBackwards(grid, path :+ minCostMove)
//      }
//    }
//    moveBackwards(grid, List((endx, endy)))
//  }
//
//  val path = playGame(grid = xxx)
//
//  def scorePath(path: List[(Int, Int)]) = {
//    val score = path
//      .drop(1)
//      .foldLeft(0)({
//        case (cum, (x, y)) =>
//          cum + xxx(y)(x)
//      })
//    println(score)
//    score
//  }
//  println(path)
//  scorePath(path)
//
//  println(costToZeroZero(xxx, 2, 0))

  println("None of this works")

}
