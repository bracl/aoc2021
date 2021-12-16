package solutions

import utils.IO._

import scala.annotation.tailrec

object day16 extends App {

  val outerMostPacket = readFile(16).head

  val ex0 = "EE00D40C823060"
  val ex1 = "8A004A801A8002F478"
  val ex2 = "620080001611562C8802118E34"
  val ex3 = "C0015000016115A2E0802F182340"
  val ex4 = "A0016C880162017C3686B18A3D4780"

  trait Value {
    def value(): Long
  }

  case class Literal(version: Int, typeId: Int, literal: Long) extends Value {
    override def value(): Long = literal
  }

  //  Packets with type ID 0 are sum packets - their value is the sum of the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
  //    Packets with type ID 1 are product packets - their value is the result of multiplying together the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
  //    Packets with type ID 2 are minimum packets - their value is the minimum of the values of their sub-packets.
  //    Packets with type ID 3 are maximum packets - their value is the maximum of the values of their sub-packets.
  //    Packets with type ID 5 are greater than packets - their value is 1 if the value of the first sub-packet is greater than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
  //    Packets with type ID 6 are less than packets - their value is 1 if the value of the first sub-packet is less than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
  //    Packets with type ID 7 are equal to packets - their value is 1 if the value of the first sub-packet is equal to the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.

  case class Operator(version: Int, typeId: Int, subpackets: List[Value]) extends Value {

    def value(): Long =
      typeId match {
        case 0 => subpackets.map(_.value()).sum
        case 1 => subpackets.map(_.value()).product
        case 2 => subpackets.map(_.value()).min
        case 3 => subpackets.map(_.value()).max
        case 5 => if (subpackets.head.value > subpackets(1).value) 1 else 0
        case 6 => if (subpackets.head.value < subpackets(1).value) 1 else 0
        case 7 => if (subpackets.head.value == subpackets(1).value) 1 else 0
      }
  }

  val lookup = Map(
    "0" -> "0000",
    "1" -> "0001",
    "2" -> "0010",
    "3" -> "0011",
    "4" -> "0100",
    "5" -> "0101",
    "6" -> "0110",
    "7" -> "0111",
    "8" -> "1000",
    "9" -> "1001",
    "A" -> "1010",
    "B" -> "1011",
    "C" -> "1100",
    "D" -> "1101",
    "E" -> "1110",
    "F" -> "1111"
  )

  def toDecimal(s: String) = BigInt(s, 2).toString(10).toInt

  def literalValue(input: String): (String, String) = {
    @tailrec
    def loop(nextInput: String, acc: List[String]): (String, List[String]) = {
      val (nextFive, rest) = nextInput.splitAt(5)
      nextFive.head match {
        case '0' => (rest, acc :+ nextFive)
        case '1' => loop(rest, acc :+ nextFive)
      }
    }
    val (rest, literalBits) = loop(input, Nil)

    val literalBinary = literalBits.map { b =>
      b.drop(1)
    }.mkString("")

    val literalDecimal = BigInt(literalBinary, 2).toString(10)

    (literalDecimal, rest)
  }

  def subpacketsZero(input: String) = subpacket(input, 15)

  def subpacketsOne(input: String) = {
    val (numberOfSubpacketsBinary, packetsThenRest) = input.splitAt(11)
    val numberOfSubpacketsDecimal = toDecimal(numberOfSubpacketsBinary)

    val (rest, subpackets) = parseOuter(packetsThenRest, numberOfSubpacketsDecimal)
    (subpackets, rest)
  }

  def subpacket(input: String, n: Int) = {
    val (lengthOfOperatorBinary, packetThenRest) = input.splitAt(n)
    val lengthOfOperatorDecimal = toDecimal(lengthOfOperatorBinary)
    val (packet, rest) = packetThenRest.splitAt(lengthOfOperatorDecimal)

    val (_, subpackets) = parseOuter(packet)
    (subpackets, rest)
  }

  def parseOuter(outer: String, numberToFind: Int = -1): (String, List[Value]) = {

    def loop(remaining: String, acc: List[Value], stopAtZero: Int = -1): (String, List[Value]) = {
      if (stopAtZero == 0) return (remaining, acc)
      remaining match {
        case s if s.matches("0+") => ("", acc)
        case ""                   => ("", acc)
        case x =>
          val (version, rest) = x.splitAt(3)
          val (typeId, rest2) = rest.splitAt(3)
          typeId match {
            case "100" => // LiteralValue
              val (decimal, remainder) = literalValue(rest2)
              val literal = Literal(toDecimal(version), toDecimal(typeId), decimal.toLong)
              loop(remainder, acc :+ literal, stopAtZero - 1)
            case _ => //operator
              val (lengthTypeId, rest3) = rest2.splitAt(1)
              lengthTypeId match {
                case "0" => // next 15 bits
                  val (subpackets, remainder) = subpacketsZero(rest3)
                  val operator = Operator(toDecimal(version), toDecimal(typeId), subpackets)
                  loop(remainder, acc :+ operator, stopAtZero - 1)
                case "1" => // number of subpackets
                  val (subpackets, remainder) = subpacketsOne(rest3)
                  val operator = Operator(toDecimal(version), toDecimal(typeId), subpackets)
                  loop(remainder, acc :+ operator, stopAtZero - 1)

              }
          }
      }
    }

    loop(outer, List[Value](), numberToFind)
  }

  val hexAsBin: String = BigInt(outerMostPacket, 16).toString(2)
  val manualHex: String = ex3.toList.map(c => lookup(c.toString)).mkString("")
  val mod = 4 - hexAsBin.length % 4
  val withPadding = if (mod < 4) List.fill(mod)("0").mkString("") + hexAsBin else manualHex
  assert(withPadding.length % 4 == 0)
  assert(manualHex.length % 4 == 0)
  val hexPackets = parseOuter(withPadding)._2

  def sumVersions(lv: List[Value]): Int =
    lv.foldLeft(0)({
      case (cumSum, v) =>
        v match {
          case l: Literal  => cumSum + l.version
          case o: Operator => cumSum + o.version + sumVersions(o.subpackets)
        }
    })

  println(hexPackets)
  println(sumVersions(hexPackets))
  println(hexPackets.head.value())

}
