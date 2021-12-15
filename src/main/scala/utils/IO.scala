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

  def updateMatrix[T](mat: List[List[T]], x: Int, y: Int, value: T): List[List[T]] =
    mat.updated(y, mat(y).updated(x, value))

  def printGrid[T](grid: List[List[T]], delim: String = ","): Unit =
    println(grid.map(v => v.mkString(delim)).mkString("\n") + "\n")

  def printGrid[T](grid: Vector[Vector[T]]): Unit =
    printGrid(grid.map(_.toList).toList)

  def updateMap[A, B](m: Map[A, B], key: A, value: B): Map[A, B] =
    m.map({
      case (k, _) if k == key => (key, value)
      case (a, b)             => (a, b)
    })

}
