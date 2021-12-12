package solutions

import utils.IO.readFile

object day12 extends App {

  type Cave = Map[String, List[String]]

  def caveFromInput(lines: List[String]): Map[String, List[String]] =
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

  def findPaths(
    cave: Cave,
    startPaths: Set[String] = Set("start"),
    func: (String, Cave) => Set[String]
  ): Set[String] = {

    def loop(cave: Cave, paths: Set[String]): Set[String] =
      paths.flatMap({
        case x if x.split(caveSeparator).last == "end" => Set(x)
        case x                                         => loop(cave, func(x, cave))
      })

    loop(cave, startPaths)
  }

  println(1, findPaths(cave, func = availableMoves).size)
  println(2, findPaths(cave, func = availableMovesPart2).size)

}
