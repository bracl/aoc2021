package utils

object IO {

  def readFile(day: Int): Seq[String] = readFile(s"/Users/bradleyking/dev/aoc2021/src/main/scala/input/$day.txt")

  def readFile(filename: String): Seq[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    lines
  }

  def readInts(day: Int): Seq[Int] = readInts(s"/Users/bradleyking/dev/aoc2021/src/main/scala/input/$day.txt")

  def readInts(filename: String): Seq[Int] = {
    val strings = readFile(filename)
    strings.map(s => s.toInt)
  }

}
