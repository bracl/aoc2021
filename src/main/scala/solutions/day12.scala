package solutions

import utils.IO.readFile

import scala.annotation.tailrec

object day12 extends App {

  type Cave = Map[String, List[String]]

  def caveFromInput(lines: List[String]) =
    lines
      .map(_.split("-"))
      .flatMap(l => List((l.head, l.last), (l.last, l.head)))
      .groupBy(_._1)
      .map({ case (k, vv) => k -> vv.map(_._2) })

  val caveLayout = readFile(12)

  val smallExample = List(
    "start-A",
    "start-b",
    "A-c",
    "A-b",
    "b-d",
    "A-end",
    "b-end"
  )

  val smallCave: Cave = caveFromInput(smallExample)
  val cave = caveFromInput(caveLayout)

  val caveSeparator = "->"

  def availableMoves(path: String, cave: Cave): Set[String] = {
    val nodes = path.split(caveSeparator)
    val possibleMoves = cave(nodes.last).filterNot(n => n.forall(_.isLower) && nodes.contains(n))
    possibleMoves.map(n => s"$path$caveSeparator$n").toSet
  }

  def alreadyTwo(strings: Array[String]): Boolean = {
    val lowers = strings.filter(_.forall(_.isLower))
    lowers.length != lowers.distinct.length
  }

  def availableMovesPart2(path: String, cave: Cave): Set[String] = {
    val nodes = path.split(caveSeparator)
    val possibleMoves =
      if (alreadyTwo(nodes)) cave(nodes.last).filterNot(n => n.forall(_.isLower) && nodes.contains(n))
      else cave(nodes.last)
    possibleMoves.filterNot(_ == "start").map(n => s"$path$caveSeparator$n").toSet
  }

  def findPathsPart1(cave: Cave, startPaths: Set[String] = Set("start")) = {

    def loop(cave: Cave, paths: Set[String]): Set[String] =
      paths.flatMap({
        case x if x.split(caveSeparator).last == "end" => Set(x)
        case x                                         => loop(cave, availableMoves(x, cave))
      })

    loop(cave, startPaths)
  }

  def findPathsPart2(cave: Cave, startPaths: Set[String] = Set("start")) = {

    def loop(cave: Cave, paths: Set[String]): Set[String] =
      paths.flatMap({
        case x if x.split(caveSeparator).last == "end" => Set(x)
        case x                                         => loop(cave, availableMovesPart2(x, cave))
      })

    loop(cave, startPaths)
  }

  println(1, findPathsPart1(cave).size)
  println(2, findPathsPart2(cave).size)

}
