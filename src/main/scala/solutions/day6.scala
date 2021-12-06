package solutions

import utils.IO.readFile

import scala.annotation.tailrec

object day6 extends App {

  val initAges = readFile(6).head.split(",").map(_.toInt).toList

  trait Fish {
    def day(): List[LanternFish]
  }

  case class LanternFish(daysToExpire: Int) extends Fish {
    def day(): List[LanternFish] = daysToExpire match {
      case 0 => List(LanternFish(6), LanternFish(8))
      case _ => List(LanternFish(daysToExpire - 1))
    }
  }

  def playGame(fish: List[LanternFish], daysLeft:Int) = {

    @tailrec
    def loop(fish:List[LanternFish], daysLeft:Int): List[LanternFish] = {
      daysLeft match {
        case 0 => fish
        case _ =>
          val newFish = fish.flatMap(f => f.day())
          loop(newFish, daysLeft-1)
      }
    }

    loop(fish, daysLeft)
  }

  def betterFishToNumber(bf: (Double, Double, Double, Double, Double, Double, Double, Double, Double)): Double = bf.productIterator.toList.map(_.asInstanceOf[Double]).sum

  def playGameBetter(initAges: List[Int], daysLeft:Int) : (Double, Double, Double, Double, Double, Double, Double, Double, Double)= {

    val zero = initAges.count(_ == 0)
    val one = initAges.count(_ == 1)
    val two = initAges.count(_ == 2)
    val three = initAges.count(_ == 3)
    val four = initAges.count(_ == 4)
    val five = initAges.count(_ == 5)
    val six = initAges.count(_ == 6)
    val seven = initAges.count(_ == 7)
    val eight = initAges.count(_ == 8)

    @tailrec
    def loop(zero:Double, one:Double, two:Double, three:Double, four:Double, five:Double, six:Double, seven:Double, eight:Double, daysLeft:Double): (Double, Double, Double, Double, Double, Double, Double, Double, Double) = {
      daysLeft match {
        case 0 => (zero, one, two, three, four, five, six, seven, eight)
        case _ =>
          val nextZero = one
          val nextOne = two
          val nextTwo = three
          val nextThree = four
          val nextFour = five
          val nextFive = six
          val nextSix = seven + zero
          val nextSeven = eight
          val nextEight = zero
          val a = (nextZero, nextOne, nextTwo, nextThree, nextFour, nextFive, nextSix, nextSeven, nextEight)
//          println(a)
//          println(betterFishToNumber(a))
          loop(nextZero, nextOne, nextTwo, nextThree, nextFour, nextFive, nextSix, nextSeven, nextEight, daysLeft-1)
      }
    }
    loop(zero, one, two, three, four, five, six, seven, eight, daysLeft)
  }

  val test = List(3,4,3,1,2)
  val testAnswer = playGameBetter(test, 18)

  val initialFish = initAges.map(x => LanternFish(x))
  val eightyDayFish = playGame(initialFish, 80)

  val betterEightyDayFish = playGameBetter(initAges, 80)
  assert(eightyDayFish.length == betterFishToNumber(betterEightyDayFish))
  println(s"80 Days: ${eightyDayFish.length}")

  val betterTwoFiveSixDayFish = playGameBetter(initAges, 256)
  println(String.format("256 Days: %.0f", betterFishToNumber(betterTwoFiveSixDayFish)))

}
