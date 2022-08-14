package solutions

import utils.IO._

import scala.annotation.tailrec
import scala.util.Try

object day22 extends App {

  type RC = Map[(Int, Int, Int), Int]
  type Block = ((Int, Int), (Int, Int), (Int, Int))
  type Instruction = (Boolean, Block)

  val xxx = readFile(22)

  val test = List(
    "on x=-20..26,y=-36..17,z=-47..7",
    "on x=-20..33,y=-21..23,z=-26..28",
    "on x=-22..28,y=-29..23,z=-38..16",
    "on x=-46..7,y=-6..46,z=-50..-1",
    "on x=-49..1,y=-3..46,z=-24..28",
    "on x=2..47,y=-22..22,z=-23..27",
    "on x=-27..23,y=-28..26,z=-21..29",
    "on x=-39..5,y=-6..47,z=-3..44",
    "on x=-30..21,y=-8..43,z=-13..34",
    "on x=-22..26,y=-27..20,z=-29..19",
    "off x=-48..-32,y=26..41,z=-47..-37",
    "on x=-12..35,y=6..50,z=-50..-2",
    "off x=-48..-32,y=-32..-16,z=-15..-5",
    "on x=-18..26,y=-33..15,z=-7..46",
    "off x=-40..-22,y=-38..-28,z=23..41",
    "on x=-16..35,y=-41..10,z=-47..6",
    "off x=-32..-23,y=11..30,z=-14..3",
    "on x=-49..-5,y=-3..45,z=-29..18",
    "off x=18..30,y=-20..-8,z=-3..13",
    "on x=-41..9,y=-7..43,z=-33..15",
    "on x=-54112..-39298,y=-85059..-49293,z=-27449..7877",
    "on x=967..23432,y=45373..81175,z=27513..53682"
  )

  def toInstructions(ll: List[String]): List[(Boolean, Block)] =
    ll.map({
      case s"on x=$xLower..$xHigher,y=$yLower..$yHigher,z=$zLower..$zHigher" =>
        (true, ((xLower.toInt, xHigher.toInt), (yLower.toInt, yHigher.toInt), (zLower.toInt, zHigher.toInt)))
      case s"off x=$xLower..$xHigher,y=$yLower..$yHigher,z=$zLower..$zHigher" =>
        (false, ((xLower.toInt, xHigher.toInt), (yLower.toInt, yHigher.toInt), (zLower.toInt, zHigher.toInt)))
    })

  val testInstructions = toInstructions(test)
  val instructions = toInstructions(xxx)

  val initReactorCore: RC = Map()

  def filterInRange(instructions: List[(Boolean, Block)]): Seq[(Boolean, Block)] =
    instructions.filter({
      case (_, ((xl, xh), (yl, yh), (zl, zh))) =>
        xl >= -50 &&
          yl >= -50 &&
          zl >= -50 &&
          xh <= 50 &&
          yh <= 50 &&
          zh <= 50
    })

  def turnOn(reactorCore: RC, block: Block): RC = {
    val ((xl, xh), (yl, yh), (zl, zh)) = block
    val currentlyOn = reactorCore.keySet
    val ops = for {
      x <- xl to xh
      y <- yl to yh
      z <- zl to zh
      if !currentlyOn.contains((x, y, z))
    } yield (x, y, z)
    val added = ops.foldLeft(reactorCore)({
      case (m, c) => m.updated(c, 1)
    })
    added
  }

  def turnOff(reactorCore: RC, block: Block): RC = {
    val ((xl, xh), (yl, yh), (zl, zh)) = block
    val currentlyOn = reactorCore.keySet
    val ops = for {
      x <- xl to xh
      y <- yl to yh
      z <- zl to zh
      if currentlyOn.contains((x, y, z))
    } yield (x, y, z)
    val removed = ops.foldLeft(reactorCore)({
      case (m, c) => m.removed(c)
    })
    removed
  }

  def part1(): Unit = {
    val filteredInstructions = filterInRange(instructions)

    val reactorCore = filteredInstructions.foldLeft((initReactorCore, 1))({
      case ((reactorCore, i), (onOff, ((xl, xh), (yl, yh), (zl, zh)))) =>
//        println(i)
        val updatedRC =
          if (onOff) turnOn(reactorCore, ((xl, xh), (yl, yh), (zl, zh)))
          else turnOff(reactorCore, ((xl, xh), (yl, yh), (zl, zh)))
        (updatedRC, i + 1)
    })

    println(reactorCore._1.size)
  }

  def min(a: Int, b: Int): Int = List(a, b).min
  def max(a: Int, b: Int): Int = List(a, b).max

  // signed volumes to track intersections
  // https://www.reddit.com/r/adventofcode/comments/rlxhmg/2021_day_22_solutions/hpizza8/

  def processInstruction(instruction: Instruction, cubes: Map[Block, Int]): Map[Block, Int] = {
    val u = Map[Block, Int]()
    val (onOff, c) = instruction
    val sign = if (onOff) 1 else -1
    val updates = cubes.toList.foldLeft((onOff, c, u))({
      case ((onOff, newBlock, update), (existingBlock, existingOnOff)) =>
        val ((nxl, nxh), (nyl, nyh), (nzl, nzh)) = newBlock
        val ((exl, exh), (eyl, eyh), (ezl, ezh)) = existingBlock
        val ixl = max(nxl, exl)
        val ixh = min(nxh, exh)
        val iyl = max(nyl, eyl)
        val iyh = min(nyh, eyh)
        val izl = max(nzl, ezl)
        val izh = min(nzh, ezh)
        val updatedUpdate =
          if (ixl <= ixh && iyl <= iyh && izl <= izh) {
            val t = ((ixl, ixh), (iyl, iyh), (izl, izh))
            val num = Try(update(t)).getOrElse(0)
            val newNum = num - existingOnOff
            update.removed(t).updated(t, newNum)
          } else update
        (onOff, newBlock, updatedUpdate)
    })
    val updatesAgain = if (onOff) {
      val t = Try(updates._3(c)).getOrElse(0)
      updates._3.removed(c).updated(c, t + sign)
    } else updates._3

    val newCubes = updatesAgain.foldLeft(cubes)({
      case (cubeMap, (b, i)) =>
        val num = Try(cubeMap(b)).getOrElse(0)
        val newNum = num + i
        cubeMap.removed(b).updated(b, newNum)
    })
    newCubes
  }

  @tailrec
  def process(
    instructions: List[Instruction],
    blocks: Map[Block, Int] = Map()
  ): Map[Block, Int] =
    instructions match {
      case Nil => blocks
      case instruction :: tail =>
        val newBlocks = processInstruction(instruction, blocks)
        process(tail, newBlocks)
    }

  def part2(): Unit = {
    val blocks = process(instructions)

    val on: Long = blocks
      .map({
        case (((xl, xh), (yl, yh), (zl, zh)), i) =>
          (xh - xl + 1).toLong * (yh - yl + 1).toLong * (zh - zl + 1).toLong * i
      })
      .sum
    println(on)
  }

  part1()
  part2()

  //         9223372036854775808
  //         2758514936282235
  //         2554587152432605
  //         28317415939

  // too low 22780737767
  //         39769202357779

}
