package solutions

import utils.IO._

import scala.util.Try

object day14 extends App {

  val xxx = readFile(14)

  val yyy = List(
    "NNCB",
    "",
    "CH -> B",
    "HH -> N",
    "CB -> H",
    "NH -> C",
    "HB -> C",
    "HC -> B",
    "HN -> C",
    "NN -> C",
    "BH -> H",
    "NC -> B",
    "NB -> B",
    "BN -> B",
    "BB -> N",
    "BC -> B",
    "CC -> N",
    "CN -> C"
  )

  val startingPoint = xxx.head

  val instructions1: Map[String, String] =
    xxx.drop(2).map(_.trim().split(" -> ")).map(l => (l.head, s"${l.head.charAt(0)}${l.last}${l.head.charAt(1)}")).toMap

  val instructions: Map[String, String] =
    xxx.drop(2).map(_.trim().split(" -> ")).map(l => (l.head, s"${l.last}${l.head.charAt(1)}")).toMap
  println(startingPoint)
  println(instructions)

  def scanString(polymer: String, instructions: Map[String, String]) = {
    def loop(polymer: String, instructions: Map[String, String], index: Int = 0): String =
      polymer.length match {
        case y if y <= index => polymer
        case x =>
          val (start, end) = polymer.splitAt(index + 1)
          val (head, inspect) = start.splitAt(start.length - 2)

          val replacedInspect = Try(instructions(inspect)).getOrElse(inspect)
          //          println(s"Looking at $inspect : becomes $replacedInspect")
          val newPolymer = s"$head$replacedInspect$end"
          //          println(newPolymer)

          loop(newPolymer, instructions, index + replacedInspect.length - 1)
      }

    loop(polymer, instructions, 1)
  }

  val res = scanString(startingPoint, instructions1)
  val res2 = scanString(res, instructions1)
  val res3 = scanString(res2, instructions1)
  println(1, res)
  println(2, res2)
  println(3, res3)

  val counts = instructions.map({ case (k, _) => (k, 0L) })

  val initialCounts = startingPoint
    .sliding(2)
    .foldLeft(counts)({
      case (map, pair) => updateMap(map, pair, map(pair) + 1)
    })

  def maxMinMin(m: Map[String, Long]): Long = {
    val charMap =
      initialCounts.foldLeft("")({ case (s, (k, v)) => s + k }).toList.distinct.map(c => c.toString -> 0L).toMap

    val charCounts = m.foldLeft(charMap)({
      case (countsMap, (k, v)) =>
        val update1 = updateMap(countsMap, k.charAt(0).toString, countsMap(k.charAt(0).toString) + v)
//        val update2 = updateMap(update1, k.charAt(1).toString, update1(k.charAt(1).toString) + v)
        update1
    })

    val realCharCount = updateMap(charCounts, startingPoint.last.toString, charCounts(startingPoint.last.toString) + 1)

    realCharCount.maxBy(_._2)._2 - realCharCount.minBy(_._2)._2
  }

  (1 to 40).foldLeft(initialCounts)({
    case (mapp, scan) =>
//      println(s"Scan $scan")
      val newMapp = mapp.foldLeft(counts)({
        case (countingMap, (pattern, count)) =>
          val newPattern = instructions(pattern)
          val update1 = updateMap(countingMap, newPattern, countingMap(newPattern) + count)

          val otherPatternToUpdate = s"${pattern.charAt(0)}${newPattern.charAt(0)}"
          val update2 = updateMap(update1, otherPatternToUpdate, update1(otherPatternToUpdate) + count)

//          if (count != 0) println(s"$pattern : Adding $count to patterns $otherPatternToUpdate $newPattern")
          update2
      })
      if (List(10, 40).contains(scan)) println(scan, maxMinMin(newMapp))
      newMapp
  })

}
