package solutions

import utils.IO._

object day21 extends App {

  val (p1Start, p2Start) = readFile(21)
    .map({
      case s"Player $i starting position: $d" => d
    })
    .splitAt(1)

  def playDeterministicDice(p1Start: Int, p2Start: Int) = {}

  println(p1Start)
  println(p2Start)

}
