package solutions

import utils.IO.readFile

object day2 extends App {

  case class Submarine(horizontal: Int = 0, depth: Int = 0) {}

  val instructions = readFile(2)

  val test = Seq(
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2"
  )

  def runGame(h1: Int, d1: Int, h2: Int, d2: Int, a: Int, instruction: String) = {
    val direction = instruction.split(" ")(0)
    val magnitude = instruction.split(" ")(1).toInt

    val (horizontal1, horizontal2, depth1, depth2, aim) = direction match {
      case "forward" => (h1 + magnitude, h2 + magnitude, d1, d2 + a * magnitude, a)
      case "down"    => (h1, h2, d1 + magnitude, d2, a + magnitude)
      case "up"      => (h1, h2, d1 - magnitude, d2, a - magnitude)
    }

//    println(s"$h1 $h2 $d1 $d2 $a => $instruction => $horizontal1 $horizontal2 $depth1 $depth2 $aim")

    (horizontal1, depth1, horizontal2, depth2, aim)
  }

  def loop(instructions: Seq[String], h1: Int, d1: Int, h2: Int, d2: Int, a2: Int): (Int, Int) = {

    val (horizontal1, depth1, horizontal2, depth2, aim2) = runGame(h1, d1, h2, d2, a2, instructions.head)

    val result: (Int, Int) = instructions match {
      case x :: Nil => (horizontal1 * depth1, horizontal2 * depth2)
      case x :: xs  => loop(xs, horizontal1, depth1, horizontal2, depth2, aim2)
    }
    result
  }

  val result = loop(instructions, 0, 0, 0, 0, 0)
  println(result)
}
