package solutions

import utils.IO._

import scala.annotation.tailrec
import scala.language.implicitConversions

object day18 extends App {

  val snailNumbers = readFile(18)

  val a = "[1,2]"
  val b = "[[1,2],3]"
  val c = "[9,[8,7]]"
  val d = "[[1,9],[8,5]]"
  val e = "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
  val f = "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
  val g = "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"

  def middleCommaIndex(str: String): Int = {
    if (str.isEmpty) return -1
    assert(str.startsWith("["))
    assert(str.endsWith("]"))
    val withoutOuterBrackets = str.drop(1).dropRight(1)
    val commaIndexes = withoutOuterBrackets.zipWithIndex
      .map({
        case (',', i) => i
        case (_, _)   => -1
      })
      .filter(_ >= 0)
    val middleIndex: List[Int] = commaIndexes.flatMap { index =>
      val (left, right) = withoutOuterBrackets.splitAt(index)
      val bracketsBothSides =
        (left.contains('[') || left.contains(']')) && (right.contains('[') || right.contains(']'))
      if (bracketsBothSides)
//        println("Not a middle comma")
        None
      else
        Some(index)
    }.toList
    if (middleIndex.length == 1)
//      println(s"we found it lads -> ${middleIndex.head + 1}") // because we remove the [ from the start of the string
      middleIndex.head + 1
    else {
//      println("deploying secondary tactic")
      val sameNumberOfOpensAndClosing = commaIndexes.flatMap { i =>
        val (left, right) = withoutOuterBrackets.splitAt(i)
        val leftOpening = left.count(_ == '[')
        val rightOpening = right.count(_ == '[')
        val leftClosing = left.count(_ == ']')
        val rightClosing = right.count(_ == ']')
        if (rightOpening == rightClosing && leftClosing == leftOpening) Some(i)
        else None
      }
      if (sameNumberOfOpensAndClosing.length == 1)
//        println(
//          s"we found it lads -> ${sameNumberOfOpensAndClosing.head + 1}"
//        ) // because we remove the [ from the start of the string
        sameNumberOfOpensAndClosing.head + 1
      else {
        println(s"Couldn't find a middle comma from $str")
        -1
      }

    }
  }

  assert(middleCommaIndex(a) == 2, "a")
  assert(middleCommaIndex(b) == 6, "b")
  assert(middleCommaIndex(c) == 2, "c")
  assert(middleCommaIndex(d) == 6, "d")
  assert(middleCommaIndex(e) == 30, "e")
  assert(middleCommaIndex(f) == 22, "f")
  assert(middleCommaIndex(g) == 30, "g")

  def printSnailNumber(sn: SnailNumber): Unit = println(sn)
  def printSnailNumber(s: String): Unit = println(readSnailNumber(s))

  def readSnailNumber(str: String): SnailNumber = {
    def loop(string: String): SnailNumber = {
      val middleComma = middleCommaIndex(string)
      middleComma match {
        case -1 => Empty()
        case _ =>
          val (left, right) = str.splitAt(middleComma)
          val leftSn = readSnailNumber(left.drop(1))
          val rightSn = readSnailNumber(right.drop(1).dropRight(1)) // the drop is the comma that we split at
          leftSn.add(rightSn)
      }
    }

    val isNumber = str.replace(",", "").replace("[", "").replace("]", "")
    val sn =
      if (isNumber.length == 1)
        Number(isNumber.toInt)
      else loop(str)
    sn
  }

  val aa = readSnailNumber(a)
  val bb = readSnailNumber(b)
  val cc = readSnailNumber(c)
  val dd = readSnailNumber(d)
  val ee = readSnailNumber(e)
  val ff = readSnailNumber(f)
  val gg = readSnailNumber(g)

  trait SnailNumber {
    def nestedLevel: Int
    def add(sn: SnailNumber): SnailNumber
    def magnitude: Int
  }

  case class Empty() extends SnailNumber {
    override def nestedLevel: Int = 0

    override def add(sn: SnailNumber): SnailNumber = sn

    override def magnitude: Int = 0
  }

  case class Number(value: Int) extends SnailNumber {

    override def nestedLevel: Int = 0

    override def add(sn: SnailNumber): SnailNumber =
      Pair(Number(value), sn, sn.nestedLevel + 1)

    override def magnitude: Int = value

  }

  def Pair(i: Int, j: Int, level: Int): Pair = Pair(Number(i), Number(j), level)

  case class Pair(left: SnailNumber, right: SnailNumber, level: Int) extends SnailNumber {
    override def nestedLevel: Int = level

    override def add(sn: SnailNumber): SnailNumber =
      Pair(Pair(left, right, level), sn, List(level, sn.nestedLevel).max + 1)

    def replaceLeft(replacement: SnailNumber): SnailNumber = Pair(replacement, right, level)
    def replaceRight(replacement: SnailNumber): SnailNumber = Pair(left, replacement, level)

    def leftMostPair: Pair = {
      @tailrec
      def loop(sn: SnailNumber): SnailNumber =
        sn match {
          case p: Pair =>
            p.left match {
              case pp: Pair                                  => loop(pp)
              case _: Number if p.right.isInstanceOf[Pair]   => loop(p.right)
              case _: Number if p.right.isInstanceOf[Number] => sn
            }
        }
      val res = loop(this)
      assert(res.nestedLevel == 1)
      res.asInstanceOf[Pair]
    }

    override def magnitude: Int = 3 * left.magnitude + 2 * right.magnitude
  }

  val aaa = readSnailNumber("[[[[[9,8],1],2],3],4]")
  val bbb = readSnailNumber("[7,[6,[5,[4,[3,2]]]]]")
  val ccc = readSnailNumber("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")
  assert(aaa.asInstanceOf[Pair].leftMostPair == Pair(9, 8, 1))
  assert(bbb.asInstanceOf[Pair].leftMostPair == Pair(3, 2, 1))
  assert(ccc.asInstanceOf[Pair].leftMostPair == Pair(7, 3, 1))

  def digitIndexes(s: String): List[(Char, Int)] = s.zipWithIndex.filter({ case (c, _) => c.isDigit }).toList

  def explodeBefore(str: String, i: Int = 0): String = {
    val indexes = digitIndexes(str)
    if (indexes.isEmpty) str
    else {
      @tailrec
      def loop(ind: List[(Char, Int)], acc: List[(Char, Int)]): List[(Char, Int)] =
        ind match {
          case Nil                                          => acc
          case i :: ii if math.abs(i._2 - acc.last._2) == 1 => loop(ii, acc :+ i)
          case i :: _ if (i._2 - acc.last._2) != 1          => acc
        }

      val li: List[(Char, Int)] =
        if (indexes.length > 1)
          loop(indexes.reverse.tail, indexes.reverse.take(1))
        else indexes

      val (before, after) = str.splitAt(li.minBy(_._2)._2)
      val (digit, end) = after.splitAt(li.length)
      val res = s"$before${digit.toInt + i}$end"
      res
    }
  }
  assert(explodeBefore("[[[[12,12],[6,14]],[[15,0],[17,", 8) == "[[[[12,12],[6,14]],[[15,0],[25,")

  def explodeAfter(str: String, i: Int): String = {
    val indexes = digitIndexes(str)
    if (indexes.isEmpty) str
    else {
      val (before, after) = str.splitAt(indexes.head._2)
      val (digit, end) = after.splitAt(List(after.indexOf(","), after.indexOf(']')).filter(_ >= 0).min)
      val res = s"$before${digit.toInt + i}$end"
      res
    }
  }

  assert(
    explodeAfter(
      "],[13,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[8,0]]]]",
      6
    ) == "],[19,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[8,0]]]]"
  )

  def explode(snailNumber: String): (Boolean, String, Int) = {
    val alreadyExploded = false
    val rubble = "" // whats left after explosion
    val index = 0
    val scan = snailNumber.foldLeft((0, index, alreadyExploded, rubble))({
      case ((depth, i, ae, r), '[') =>
        val nd = depth + 1
        if (nd == 5 && !ae) {
          //          println("boom")
          val (start, rest) = snailNumber.splitAt(i)
          val (toExplode, end) = rest.splitAt(rest.indexOf(']') + 1)

          val leftValue = toExplode.slice(1, toExplode.indexOf(",")).toInt
          val rightValue = toExplode.slice(toExplode.indexOf(",") + 1, toExplode.indexOf("]")).toInt

          val newStart = explodeBefore(start, leftValue)
          val newEnd = explodeAfter(end, rightValue)
          val zero = "0"
          val afterExplosion = s"$newStart$zero$newEnd"

          //          println(s"start={$start} toExplode={$toExplode} end={$end}")
          //          println(s"before={$snailNumber} toExplode={$afterExplosion}")
          (depth + 1, i + 1, true, afterExplosion)
        } else (depth + 1, i + 1, ae, r)
      case ((depth, i, ae, r), ']') => (depth - 1, i + 1, ae, r)
      case ((depth, i, ae, r), _)   => (depth, i + 1, ae, r)
    })
    if (scan._4.isEmpty)
      //      println("Did not explode")
      (false, snailNumber, 0)
    else (true, scan._4, 0)
  }

  def assertExplode(): Unit = {
    assert(explode("[[[[[9,8],1],2],3],4]")._2 == "[[[[0,9],2],3],4]", 1)
    assert(explode("[7,[6,[5,[4,[3,2]]]]]")._2 == "[7,[6,[5,[7,0]]]]", 2)
    assert(explode("[[6,[5,[4,[3,2]]]],1]")._2 == "[[6,[5,[7,0]]],3]", 3)
    assert(explode("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")._2 == "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", 4)
    assert(explode("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")._2 == "[[3,[2,[8,0]]],[9,[5,[7,0]]]]", 5)
    assert(
      explode(
        "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
      )._2 == "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]",
      6
    )
  }
  assertExplode()

  def split(snailNumber: String): (Boolean, String, Int) = {
    // assumes we dont have triple digit numbers...
    val r = """\d\d""".r.findFirstMatchIn(snailNumber)
    if (r.isDefined) {
      val (start, rest) = snailNumber.splitAt(r.get.start)
      val (toSplit, end) = rest.splitAt(2)
      val i = toSplit.toInt.toFloat
      val lower = math.floor(i / 2).toInt
      val upper = math.ceil(i / 2).toInt
      val replacement = s"[$lower,$upper]"
      val res = s"$start$replacement$end"
      (true, res, 1)
    } else (false, snailNumber, 1)
  }

  def assertSplit(): Unit = {
    assert(split("[[[[0,7],4],[15,[0,13]]],[1,1]]")._2 == "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]", 1)
    assert(split("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")._2 == "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]", 2)
  }
  assertSplit()

  def reduce(snailNumber: String): String = {

    val cleanActions: List[String => (Boolean, String, Int)] = List(explode, split)

    @tailrec
    def loop(snailNum: String, q: List[String => (Boolean, String, Int)]): String =
      q match {
        case Nil => snailNum
        case q :: t =>
          val (acted, sn, i) = q(snailNum)
          if (acted) {
            val newQueue: List[String => (Boolean, String, Int)] = i match {
              case 0 => cleanActions.take(1) ++ t
              case 1 => cleanActions ++ t
            }
            loop(sn.replace(" ", ""), newQueue)
          } else loop(sn, t)
      }

    val res = loop(snailNumber, cleanActions)
    res
  }

  def add(i: String, j: String): String = {
    val added = s"[$i,$j]"
    val reduced = reduce(added)
    reduced
  }

  def assertAddEquals(a: String, b: String, c: String): Unit = {
    val x = add(a, b)
    assert(x == c, x)
  }

  def assertAdd(): Unit = {
    assertAddEquals("[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

    assertAddEquals(
      "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
      "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
      "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
    )
    assertAddEquals(
      "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]",
      "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
      "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
    )
    assertAddEquals(
      "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]",
      "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
      "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"
    )

    assertAddEquals(
      "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]",
      "[7,[5,[[3,8],[1,4]]]]",
      "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"
    )

    assertAddEquals(
      "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]",
      "[[2,[2,2]],[8,[8,1]]]",
      "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"
    )

    assertAddEquals(
      "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]",
      "[2,9]",
      "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"
    )

    assertAddEquals(
      "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]",
      "[1,[[[9,3],9],[[9,0],[0,7]]]]",
      "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]"
    )

    assertAddEquals(
      "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]",
      "[[[5,[7,4]],7],1]",
      "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]"
    )

    assertAddEquals(
      "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]",
      "[[[[4,2],2],6],[8,7]]",
      "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
    )
  }

  assertAdd()

  def addList(snailNumbers: List[String]): String =
    if (snailNumbers.isEmpty) {
      println("whats going on here...")
      ""
    } else
      snailNumbers.tail.foldLeft(snailNumbers.head)({
        case (current, nextSnailNumber) => add(current, nextSnailNumber)
      })

  def testList(n: Int): List[String] = (1 to n).map(x => s"[$x,$x]").toList

  val l1 = testList(4)
  val l2 = testList(5)
  val l3 = testList(6)

  val l4 = List(
    "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
    "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
    "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
    "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
    "[7,[5,[[3,8],[1,4]]]]",
    "[[2,[2,2]],[8,[8,1]]]",
    "[2,9]",
    "[1,[[[9,3],9],[[9,0],[0,7]]]]",
    "[[[5,[7,4]],7],1]",
    "[[[[4,2],2],6],[8,7]]"
  )

  def assertList(): Unit = {
    assert(addList(l1) == "[[[[1,1],[2,2]],[3,3]],[4,4]]", 1)
    assert(addList(l2) == "[[[[3,0],[5,3]],[4,4]],[5,5]]", 2)
    assert(addList(l3) == "[[[[5,0],[7,4]],[5,5]],[6,6]]", 3)
    assert(addList(l4) == "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 4)
  }
  assertList()

  def assertAlreadyReduced(): Unit = {
    val v = "[[[[4,0],[5,4]],[[7,7],[6,5]]],[[[0,6],[6,5]],[[6,7],[7,6]]]]"
    assert(v == explode(v)._2)
    assert(v == split(v)._2)
  }
  assertAlreadyReduced()

  val sumAllSnailNumbers: String = addList(snailNumbers)
  val resultSnailNumber = readSnailNumber(sumAllSnailNumbers)
  val pt1 = resultSnailNumber.magnitude
  assert(pt1 == 4433)
  println(pt1)

  val allSumCombinations: List[String] = for {
    x <- snailNumbers
    y <- snailNumbers
  } yield if (x != y) addList(List(x, y)) else ""

  val allMagnitudes = allSumCombinations.filter(_.nonEmpty).map(sn => readSnailNumber(sn).magnitude)
  val pt2 = allMagnitudes.max
  assert(pt2 == 4559)
  println(pt2)
}
