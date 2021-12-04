package utils

object IO {

  def readFile(day: Int): List[String] = readFile(s"/Users/bradleyking/dev/aoc2021/src/main/scala/input/$day.txt")

  def readFile(filename: String): List[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line.trim).toList
    bufferedSource.close
    lines
  }

  def readInts(day: Int): List[Int] = readInts(s"/Users/bradleyking/dev/aoc2021/src/main/scala/input/$day.txt")

  def readInts(filename: String): List[Int] = {
    val strings = readFile(filename)
    strings.map(s => s.toInt)
  }

}
