package solutions

import utils.IO._

import scala.util.Try

object day20 extends App {

  type ImageMap = Map[(Int, Int), Int]
  val initMap: ImageMap = Map()

  val xxx = readFile(20)

  val enhancementAlgorithm = xxx.take(1).head
  val inputImage: Vector[Vector[Char]] = xxx.drop(2).map(_.trim().toVector).toVector

  def imageToMap(input: Vector[Vector[Char]]): ImageMap = {
    val R = inputImage.size
    val C = inputImage.head.size
    assert(R == C)
    (0 until input.size * input.head.size).foldLeft(initMap)({
      case (m, i) =>
        val row = i % R
        val col = (i.toFloat / R).floor.toInt
        val pixel: Char = input(row)(col)
        val updatedMap: Map[(Int, Int), Int] = pixel match {
          case '.' => m + ((col, row) -> 0)
          case '#' => m + ((col, row) -> 1)
        }
        updatedMap
    })
  }

  val inputMap = imageToMap(inputImage)

  def mapBounds(image: ImageMap): (Int, Int, Int, Int) = {
    val r = image.keys.minBy(_._1)._1
    val rr = image.keys.maxBy(_._1)._1
    val c = image.keys.minBy(_._2)._2
    val cc = image.keys.maxBy(_._2)._2
    (r, c, rr, cc)
  }

  def toDecimal(s: String): Int = BigInt(s, 2).toString(10).toInt

  val DC = List(-1, 0, 1, -1, 0, 1, -1, 0, 1)
  val DR = List(-1, -1, -1, 0, 0, 0, 1, 1, 1)

  def outputValue(index: Int, enhancementAlgorithm: String = day20.enhancementAlgorithm): Int =
    enhancementAlgorithm(index) match {
      case '.' => 0
      case '#' => 1
    }

  def inputSquare(input: ImageMap, x: Int, y: Int, defaultValue: Int = 0): Int = {
    val sequence = (0 to 8).map(i => (x + DC(i), y + DR(i)))
    val binaryString = sequence.map(ij => Try(input(ij)).getOrElse(defaultValue)).mkString("")
    toDecimal(binaryString)
  }

  def process(image: ImageMap, enhancementString: String = enhancementAlgorithm, defaultValue: Int = 0): ImageMap = {
    val (r, c, rr, cc) = mapBounds(image)
//    println(r, c, rr, cc)
    val outputSequence = for {
      y <- c - 1 to cc + 1
      x <- r - 1 to rr + 1
    } yield (x, y, outputValue(inputSquare(image, x, y, defaultValue), enhancementString))
    val outputMap = outputSequence.foldLeft(initMap)({
      case (m, (x, y, 1)) => m + ((x, y) -> 1)
      case (m, (x, y, 0)) => m + ((x, y) -> 0)
    })
    outputMap
  }

  def numOnes(m: ImageMap): Int = m.values.count(_ == 1)

  val out = (1 to 50).foldLeft(inputMap)({
    case (image, i) =>
      if (i == 3) println(numOnes(image))
      process(image, defaultValue = 1 - (i % 2))
  })
  println(numOnes(out))
}
