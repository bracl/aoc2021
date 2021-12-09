package solutions

import utils.IO.readFile

object day3 extends App {

  val diagnosis = readFile(3)

  def mostCommonBit(in: List[String]): Seq[String] = {
    val transpose = in.map(_.toList).transpose
    val max = transpose.head.length
    val mostCommon = transpose.map { l =>
      val ones = l.count(_ == '1').toFloat
      if (ones < (max / 2.0)) "0" else "1"
    }
    mostCommon
  }

  def flip(s: String): String =
    s match {
      case "0" => "1"
      case "1" => "0"
    }

  val mostCommon: Seq[String] = mostCommonBit(diagnosis)

  val leastCommon: Seq[String] = mostCommon map flip

  def binaryToInt(s: String): Int = Integer.parseInt(s, 2)

  val gamma = binaryToInt(mostCommon.mkString)
  val epsilon = binaryToInt(leastCommon.mkString)

  println("PART 1")
  println(s"Gamma: $gamma")
  println(s"Epsilon: $epsilon")
  println(s"Power Consumption: ${gamma * epsilon}")

  def filterInput(input: List[String], most: Boolean): String = {

    def loop(in: List[String], index: Int): String = {
      val mcbb = mostCommonBit(in)
      val mcb = if (most) mcbb(index) else flip(mcbb(index))
      val filtered = in.filter(s => s(index).toString == mcb)
      val foundValue = filtered match {
        case ll :: Nil        => ll
        case ll: List[String] => loop(ll, index + 1)
        case Nil              => "Somethings gone wrong"
      }
      foundValue
    }
    loop(input, 0)
  }

  val oxygenRating = filterInput(diagnosis, most = true)
  val co2 = filterInput(diagnosis, most = false)

  println("\nPART 2")
  println(s"Oxygen: ${binaryToInt(oxygenRating)}")
  println(s"CO2: ${binaryToInt(co2)}")
  println(s"CO2 Scrubber Rating: ${binaryToInt(oxygenRating) * binaryToInt(co2)}")
}
