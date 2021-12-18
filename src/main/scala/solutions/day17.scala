package solutions

import utils.IO._

import scala.annotation.tailrec

object day17 extends App {

  val xxx = readFile(17)

  //20..30, y=-10..-5
  //  val maxTargetX = 30
  //  val minTargetX = 20
  //  val maxTargetY = -5
  //  val minTargetY = -10

  //  x=253..280, y=-73..-46
  val maxTargetX = 280
  val minTargetX = 253
  val maxTargetY = -46
  val minTargetY = -73

  //Victor
//  val maxTargetX = 250
//  val minTargetX = 206
//  val maxTargetY = -57
//  val minTargetY = -105

  val startPosition = (0, 0)

  //  The probe's x position increases by its x velocity.
  //  The probe's y position increases by its y velocity.
  //  Due to drag, the probe's x velocity changes by 1 toward the value 0; that is, it decreases by 1 if it is greater than 0, increases by 1 if it is less than 0, or does not change if it is already 0.
  //  Due to gravity, the probe's y velocity decreases by 1.

  def performStep(start: (Int, Int), dx: Int, dy: Int) = {
    val newX = start._1 + dx
    val newY = start._2 + dy
    val end = (newX, newY)
//    println(s"$start -> ($dx,$dy) -> $end")
    end
  }

  def fireProbe(startPosition: (Int, Int), x: Int, y: Int): Int = {
//    println(s"Firing probe with ($x, $y)")
    @tailrec
    def loop(pos: (Int, Int), i: Int, j: Int, ys: List[Int] = Nil): Int =
      if (pos._1 >= minTargetX && pos._1 <= maxTargetX && pos._2 <= maxTargetY && pos._2 >= minTargetY)
//        println(s"Target acquired - initial velocity ($x, $y)")
        ys.max
      else {
        val newPosition = performStep(pos, i, j)
        val dragI = i match {
          case 0            => 0
          case xx if xx > 0 => xx - 1
          case xx if xx < 0 => xx + 1
        }
        val gravityJ = j - 1
        newPosition match {
          case (x, y) if x >= maxTargetX + 1 => -999
          case (x, y) if y <= minTargetY - 1 => -999
          case (x, y) if x <= maxTargetX     => loop(newPosition, dragI, gravityJ, ys :+ y)
          case (x, y) if y >= minTargetY     => loop(newPosition, dragI, gravityJ, ys :+ y)
        }
      }

    loop(startPosition, x, y)
  }

  val weDidItBoys = for {
    x <- 2 to (maxTargetX + 1)
    y <- minTargetY - 1 to 200
  } yield fireProbe(startPosition, x, y)

  println(weDidItBoys.max)
  println(weDidItBoys.count(_ != -999))

}
