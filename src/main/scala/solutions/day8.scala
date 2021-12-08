package solutions

import utils.IO.{readFile, readInts}

object day8 extends App {

  val xxx: List[List[List[String]]] = readFile(8).map(_.split(" \\| ").toList.map(_.split(" ").toList))

  def part1(): Int = {
    val res = for {
      x <- xxx
    } yield {
      val out = x.tail.head
      out.count(n => n.length<=4 || n.length == 7)
    }
    res.sum
  }

  println(s"Part 1: ${part1()}")

  def findUniqueLength(input:List[String], length:Int): String = {
    val one = input.filter(s => s.length == length)
    assert(one.length==1)
    one.head
  }

  def findOne(input:List[String]): String = findUniqueLength(input, 2)
  def findSeven(input:List[String]): String = findUniqueLength(input, 3)
  def findFour(input:List[String]): String = findUniqueLength(input, 4)
  def findEight(input:List[String]): String = findUniqueLength(input, 7)
  def findTwo(input:List[String], ffff:String): String = {
    val fives = input.filter(s => s.length == 5)
    val two = fives.filterNot(s => s.contains(ffff))
    assert(two.length == 1)
    two.head
  }
  def findThree(input:List[String], two:String, cccc:String) = {
    val fives = input.filter(s => s.length == 5)
    val three = fives.filterNot(s => s == two).filter(s => s.contains(cccc))
    assert(three.length == 1)
    three.head
  }
  def findFive(input:List[String], two:String, three:String) = {
    val fives = input.filter(s => s.length == 5)
    val five = fives.filterNot(s => s == two || s == three)
    assert(five.length == 1)
    five.head
  }

  def findNine(input:List[String], eeee:String) = {
    val sixes = input.filter(s => s.length == 6)
    val nine = sixes.filterNot(s => s.contains(eeee))
    assert(nine.length ==1)
    nine.head

  }

  def findSix(input:List[String], cccc:String) = {
    val sixes = input.filter(s => s.length == 6)
    val six = sixes.filterNot(s => s.contains(cccc))
    assert(six.length ==1)
    six.head

  }

  def findZero(input:List[String], six:String, nine:String) = {
    val sixes = input.filter(s => s.length == 6)
    val zero = sixes.filterNot(s => s == six || s == nine)
    assert(zero.length == 1)
    zero.head

  }

  def findOneDiff(larger:String, smaller:String): String = {
    val onlyLetterLeft = larger.toSet.diff(smaller.toSet)
    assert(onlyLetterLeft.size == 1)
    onlyLetterLeft.head.toString
  }

  def findCharCount(input:List[String], occurences:Int) = {
    val counts = "abcdefg".toList.map(c => (c, input.count(s => s.toList.contains(c))))
    assert(counts.count(_._2 == occurences) == 1)
    counts.filter(_._2 == occurences).head._1.toString
  }

  def findAAAA(seven:String, one:String): String = findOneDiff(seven, one)
  def findBBBB(input:List[String]) = findCharCount(input, 6)
  def findCCCC(one:String, ffff:String):String = one.replace(ffff,"")
  def findDDDD(eight:String, zero:String) = findOneDiff(eight, zero)
  def findEEEE(input:List[String]) = findCharCount(input, 4)
  def findFFFF(input:List[String]) = findCharCount(input, 9)
  def findGGGG(four:String, aaa:String, eee:String) = {
    val res = "abcdefg".toList.filterNot(l => four.toList.contains(l) || l.toString == eee || l.toString == aaa)
    assert(res.length == 1)
    res.head.toString
  }

  def part2(): Int = {
    val firstInput = List(
      "acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"
    )
    val firstOutput = List(
      "cdfeb", "fcadb", "cdfeb", "cdbaf"
    )


    def compareSets(s1:String, s2:String) = {
      s1.toSet.diff(s2.toSet).isEmpty && s2.toSet.diff(s1.toSet).isEmpty
    }

    def calculateLine(input:List[String], output:List[String]): Int = {

      val one = findOne(input)
      val four = findFour(input)
      val seven = findSeven(input)
      val eight = findEight(input)
      val ffff = findFFFF(input)
      val aaaa = findAAAA(seven, one)
      val eeee = findEEEE(input)
      val bbbb = findBBBB(input)
      val cccc = findCCCC(one, ffff)
      val two = findTwo(input, ffff)
      val three = findThree(input, two, cccc)
      val five = findFive(input, two, three)
      val nine = findNine(input, eeee)
      val six = findSix(input, cccc)
      val zero = findZero(input, six, nine)
      val dddd = findDDDD(eight, zero)
      val gggg = findGGGG(four, aaaa, eeee)

      def outputToNumber(out:String): Int = {

        val nums = List(
          zero,
          one,
          two,
          three,
          four,
          five,
          six,
          seven,
          eight,
          nine
        )
        val number = nums.filter(n => compareSets(n, out))
        assert(number.length == 1)
        number.head match {
          case s:String if s == one => 1
          case s:String if s == two => 2
          case s:String if s == three => 3
          case s:String if s == four => 4
          case s:String if s == five => 5
          case s:String if s == six => 6
          case s:String if s == seven => 7
          case s:String if s == eight => 8
          case s:String if s == nine => 9
          case s:String if s == zero => 0
          case s:String => 9999999
        }
      }

      val outputs = output.map(outputToNumber)
      val res = outputs.mkString("").toInt
      res
    }

    val allOutputs = xxx.map({ entry =>
      val in = entry.head
      val out = entry.tail.head
      calculateLine(in, out)
    })

    allOutputs.sum
  }

  println(s"Part 2: ${part2()}")
}
