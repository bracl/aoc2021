package solutions

import utils.IO.readInts

object day1 extends App {
  val measurements = readInts(1)

  def part1(measurements: Seq[Int]) = {
    val is_bigger_than_previous = measurements.sliding(2).map(l => l.head < l(1)).toList
    val increases = is_bigger_than_previous.count(_ == true)
    increases
  }

  def part2() = {
    val windows: Seq[Int] = measurements.sliding(3).filter(_.length==3).map(l => l.sum).toSeq
    part1(windows)
  }

  println(part2())
}
