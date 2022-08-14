package solutions

import utils.IO._

import scala.annotation.tailrec
import scala.util.Try

object day25 extends App {

  type CucumberMap = Map[(Int, Int), String]
  val remove = "remove"
  val empty = "."

  val testInput = List(
    "v...>>.vv>",
    ".vv>>.vv..",
    ">>.>v>...v",
    ">>v>>.>.v.",
    "v>v.vv.v..",
    ">.>>..v...",
    ".vv..>.>v.",
    "v.v..>>v.v",
    "....v..v.>"
  )

//  val inputList = testInput.map(_.split("").toVector).toVector
  val inputList = readFile(25).map(_.split("").toVector).toVector
  val R = inputList.size
  val C = inputList.head.size
  println(s"Grid of size $R * $C = ${R * C}")

  val steps = for {
    x <- 0 until C
    y <- 0 until R
    if inputList(y)(x) != empty
  } yield (x, y, inputList(y)(x))

  val inputMap = steps.foldLeft(Map[(Int, Int), String]())({
    case (m, (x, y, v)) => m.updated((x, y), v)
  })

  def checkCoord(x: Int, y: Int, R: Int, C: Int, direction: String): (Int, Int) = {
    val newCoord = direction match {
      case ">" => if (x + 1 == C) (0, y) else (x + 1, y)
      case "v" => if (y + 1 == R) (x, 0) else (x, y + 1)
    }
    newCoord
  }

  def cucumberUpdates(m: CucumberMap, direction: String): List[(String, Int, Int)] = {
    assert(">v".contains(direction))

    (for {
      ((x, y), cucumber) <- m.toList
      if cucumber == direction
      (nx, ny) = checkCoord(x, y, R, C, cucumber)
      nc = Try(m((nx, ny))).getOrElse(empty)
      if nc == empty
    } yield List((remove, x, y), (cucumber, nx, ny))).flatten

  }

  def updateMap(cucumberMap: CucumberMap, updates: List[(String, Int, Int)]): CucumberMap =
    updates.foldLeft(cucumberMap)({
      case (m, ("remove", x, y)) => m.removed((x, y))
      case (m, (v, x, y))        => m.updated((x, y), v)
    })

  val east = ">"
  val south = "v"

  def printCucumbers(m: CucumberMap): Unit = {
    val s = (for {
      y <- 0 until R
      v = (0 until C).map(x => Try(m((x, y))).getOrElse(empty)).mkString
    } yield v).mkString("\n")
    println(s)
  }

  @tailrec
  def process(cucumberMap: CucumberMap, step: Int = 1): (CucumberMap, Int) = {
    val updatesE = cucumberUpdates(cucumberMap, east)
    val updatedMap = updateMap(cucumberMap, updatesE)
    val updatesS = cucumberUpdates(updatedMap, south)
    val updatedMap2 = updateMap(updatedMap, updatesS)

    if (updatesE.isEmpty && updatesS.isEmpty) (updatedMap2, step)
    else process(updatedMap2, step + 1)
  }

  val (settled, step) = process(inputMap)
  println(step)
  printCucumbers(settled)
}

object One extends App {

//  val nums = List.fill(10000)(Random.nextInt(10000))
//  val target = Random.nextInt(50000)
  val nums = List(5, 10, 15, 20)
  val target = 40

  def twoSum(a: Int, b: List[Int], c: Int): Boolean =
    b.exists(i => a + i == c)

  val t0 = System.currentTimeMillis()
  println(nums.zipWithIndex.exists({ case (j, index) => twoSum(j, nums.drop(index + 1), target) }))
  val t1 = System.currentTimeMillis()
  println(nums.toSet.subsets(2).map(_.sum).contains(target))
  val t2 = System.currentTimeMillis()
  println(nums.exists(i => nums.map(_ + i).contains(target)))
  val t3 = System.currentTimeMillis()

  println(t1 - t0)
  println(t2 - t1)
  println(t3 - t2)
}

object Two extends App {

  val nums = List(1, 5, 2, 11, 15, 7)
  val nums2 = List(10, 22, 16, 71, 54, 102)

  def oddOneOut(l: List[Int]): Int =
    l.map(i => (i, i % 2)).partition(_._2 == 0) match {
      case (a, _) if a.length == 1 => a.head._1
      case (_, b) if b.length == 1 => b.head._1
    }

  println(oddOneOut(nums))
  println(oddOneOut(nums2))

}
