package solutions

import utils.IO.readFile

import scala.annotation.tailrec

object day10 extends App {

  val chunks = readFile(10)

  def isCorrupted(chunk: String): (String, String, Boolean) = {

    @tailrec
    def loop(s: String, reduced: Boolean): (String, Boolean) =
      s match {
        case "" => ("", false)
        case chunksLeft if reduced =>
          val reducedChunk = reduceChunk(chunksLeft)
          loop(reducedChunk._1, reducedChunk._2)
        case chunksLeft => (chunksLeft, true)
      }

    val corrupt: (String, Boolean) = loop(chunk, reduced = true)
    (chunk, corrupt._1, corrupt._2)
  }

  def recurseReduce(chunk: String) = {
    @tailrec
    def loop(s: String, reduced: Boolean): (String, Boolean) =
      s match {
        case "" => ("", false)
        case chunksLeft if reduced =>
          val reducedChunk = reduceChunk(chunksLeft)
          loop(reducedChunk._1, reducedChunk._2)
        case chunksLeft => (chunksLeft, true)
      }

    loop(chunk, reduced = true)._1
  }

  def reduceChunk(chunk: String) = {
    val startingSize = chunk.length
    val reduced = chunk
      .replace("()", "")
      .replace("{}", "")
      .replace("<>", "")
      .replace("[]", "")
    (reduced, reduced.length < startingSize)
  }

  val points = Map(
    "]" -> 57,
    "}" -> 1197,
    ")" -> 3,
    ">" -> 25137,
    "___" -> 0
  )

  def firstIllegalCharacter(corruptedChunk: String): String = {
    val indexes: List[(Int, Char)] = List(']', '}', ')', '>').map(b => (corruptedChunk.indexOf(b), b))
    val filtered = indexes.filter(_._1 >= 0)
    if (filtered.nonEmpty)
      filtered
        .minBy(_._1)
        ._2
        .toString
    else "___"

  }

  val corrupted = chunks.map(ch => isCorrupted(ch))
  val corruptedReducedChunks = corrupted.map(_._2)

  val incomplete = corrupted.filter { ccc =>
    val illChar = firstIllegalCharacter(ccc._2)
    points(illChar) == 0
  }.map(_._1)
  val illegalCharacterPoints = corruptedReducedChunks.map(firstIllegalCharacter).map(points(_))
  println(s"part1 ${illegalCharacterPoints.sum}")

  def completeChunk(chunk: String) = {
    val reduced = recurseReduce(chunk)
    val score: Long = reduced.foldRight(0L) { (ch, cumSum) =>
      ch match {
        case '(' => (cumSum * 5) + 1
        case '[' => (cumSum * 5) + 2
        case '{' => (cumSum * 5) + 3
        case '<' => (cumSum * 5) + 4
      }
    }
    score
  }

  val scores = incomplete.map(completeChunk).sorted
  println(scores.length)
  println(scores.length / 2)
  println(scores(scores.length / 2))

//  incomplete.map(_._1).foreach(completeChunk)

}
