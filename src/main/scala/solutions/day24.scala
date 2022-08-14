package solutions

import utils.IO._

object day24 extends App {

  val inputInstructions = readFile(24)
  val l = inputInstructions.length
  val groupSize = l / 14
  val inputSections = inputInstructions.grouped(groupSize).toList

  //    inp w
  //    mul x 0
  //    add x z
  //    mod x 26
  //    div z {DIV}
  //    add x {CHECK}
  //    eql x w
  //    eql x 0
  //    mul y 0
  //    add y 25
  //    mul y x
  //    add y 1
  //    mul z y
  //    mul y 0
  //    add y w
  //    add y {OFFSET}
  //    mul y x
  //    add z y
  def parseSection(section: List[String]) = {
    val div = section(4).split(" ").last
    val check = section(5).split(" ").last
    val offset = section(15).split(" ").last
    println((div, check, offset))
    (div, check, offset)
  }

  val usefulInfo = inputSections.map(parseSection)
  val infoSections = usefulInfo.map({ case (_, c, o) => SectionInfo(c.toInt, o.toInt) })
  println(usefulInfo)

  //- Read an input.
  //- Check the condition: input == (z % 26) + {CHECK}.
  //- If {DIV} is 26 (or equivalently, {CHECK} is negative), set z = z / 26.
  //- If the condition is met, do nothing further.
  //- Otherwise, set z = 26 * z + input + {OFFSET}
  case class ALU(iv: Long, w: Long = 0, x: Long = 0, y: Long = 0, z: Long = 0, nextInputValueIndex: Int = 0) {

    def inp(variable: String) = {
      println(w, x, y, z)
      val v = iv.toString.toList(nextInputValueIndex).toString.toInt
      variable match {
        case "w" => ALU(iv, v, x, y, z, nextInputValueIndex + 1)
        case "x" => ALU(iv, w, v, y, z, nextInputValueIndex + 1)
        case "y" => ALU(iv, w, x, v, z, nextInputValueIndex + 1)
        case "z" => ALU(iv, w, x, y, v, nextInputValueIndex + 1)
      }
    }

    def add(a: String, b: Long) =
      a match {
        case "w" => ALU(iv, w + b, x, y, z, nextInputValueIndex)
        case "x" => ALU(iv, w, x + b, y, z, nextInputValueIndex)
        case "y" => ALU(iv, w, x, y + b, z, nextInputValueIndex)
        case "z" => ALU(iv, w, x, y, z + b, nextInputValueIndex)
      }

    def multiply(a: String, b: Long) =
      a match {
        case "w" => ALU(iv, w * b, x, y, z, nextInputValueIndex)
        case "x" => ALU(iv, w, x * b, y, z, nextInputValueIndex)
        case "y" => ALU(iv, w, x, y * b, z, nextInputValueIndex)
        case "z" => ALU(iv, w, x, y, z * b, nextInputValueIndex)
      }

    def divide(a: String, b: Long) = {
      assert(b != 0)
      val res = a match {
        case "w" => ALU(iv, math.floor(w.toFloat / b.toFloat).toInt, x, y, z, nextInputValueIndex)
        case "x" => ALU(iv, w, math.floor(x.toFloat / b.toFloat).toInt, y, z, nextInputValueIndex)
        case "y" => ALU(iv, w, x, math.floor(y.toFloat / b.toFloat).toInt, z, nextInputValueIndex)
        case "z" => ALU(iv, w, x, y, math.floor(z.toFloat / b.toFloat).toInt, nextInputValueIndex)
      }
      res
    }

    def mod(a: String, b: Long) = {
      assert(b >= 0)
      val res = a match {
        case "w" => ALU(iv, w % b, x, y, z, nextInputValueIndex)
        case "x" => ALU(iv, w, x % b, y, z, nextInputValueIndex)
        case "y" => ALU(iv, w, x, y % b, z, nextInputValueIndex)
        case "z" => ALU(iv, w, x, y, z % b, nextInputValueIndex)
      }
      res
    }

    def equals(a: String, b: Long) =
      a match {
        case "w" if w == b => ALU(iv, 1, x, y, z, nextInputValueIndex)
        case "w"           => ALU(iv, 0, x, y, z, nextInputValueIndex)
        case "x" if x == b => ALU(iv, w, 1, y, z, nextInputValueIndex)
        case "x"           => ALU(iv, w, 0, y, z, nextInputValueIndex)
        case "y" if y == b => ALU(iv, w, x, 1, z, nextInputValueIndex)
        case "y"           => ALU(iv, w, x, 0, z, nextInputValueIndex)
        case "z" if z == b => ALU(iv, w, x, y, 1, nextInputValueIndex)
        case "z"           => ALU(iv, w, x, y, 0, nextInputValueIndex)
      }
  }

  def process(alu: ALU, instruction: String): ALU =
    instruction match {
      case s"inp $a"                                            => alu.inp(a)
      case s"add $a $b" if "0123456789".contains(b.toList.last) => alu.add(a, b.toLong)
      case s"mul $a $b" if "0123456789".contains(b.toList.last) => alu.multiply(a, b.toLong)
      case s"div $a $b" if "0123456789".contains(b.toList.last) => alu.divide(a, b.toLong)
      case s"mod $a $b" if "0123456789".contains(b.toList.last) => alu.mod(a, b.toLong)
      case s"eql $a $b" if "0123456789".contains(b.toList.last) => alu.equals(a, b.toLong)
      case _                                                    => processString(alu, instruction)
    }

  def processString(alu: ALU, instruction: String): ALU = {
    val b = instruction.split(" ").last
    val bAsInt = b match {
      case "w" => alu.w
      case "x" => alu.x
      case "y" => alu.y
      case "z" => alu.z
    }
    val newInstruction = instruction.split(" ").dropRight(1).mkString(" ") + s" $bAsInt"
    //    println(newInstruction)
    process(alu, newInstruction)
  }

  def processNumber(inputNumber: Long, instructions: List[String]): ALU = {
    assert(instructions.head.startsWith("inp"))
    assert(instructions.nonEmpty)
    assert(
      instructions.count(_.startsWith("inp")) == inputNumber.toString.length,
      s"Input number didnt match the number of input instructions\nTo match your instructions, you need a ${instructions
        .count(_.startsWith("inp"))} digit number"
    )
    val startingALU = ALU(inputNumber)
    instructions.foldLeft(startingALU)({
      case (alu, instruction: String) =>
        //        println(alu)
        process(alu, instruction)
    })
  }

  val testInstructions1 = List(
    "inp x",
    "mul x -1"
  )

  val testInstructions2 = List(
    "inp z",
    "inp x",
    "mul z 3",
    "eql z x"
  )

  val testInstructions3 = List(
    "inp w",
    "add z w",
    "mod z 2",
    "div w 2",
    "add y w",
    "mod y 2",
    "div w 2",
    "add x w",
    "mod x 2",
    "div w 2",
    "mod w 2"
  )

  def valid(alu: ALU) =
    alu.z == 0

  case class SectionInfo(check: Int, offset: Int)

  def processNumberSmarter(inputNumber: Long, sectionInfo: List[SectionInfo]) = {
    assert(inputNumber.toString.length == 14)
    assert(sectionInfo.length == 14)
    val initZ = 0
    val initStack = List[Long]()
    val stillGood = true
    val initIndex = 0
    val inputNumberArray = inputNumber.toString.toList.map(_.toInt)
    val (z, stack, success, _) = sectionInfo.foldLeft((initZ, initStack, stillGood, initIndex))({
      case ((z, stack, sg, index), si) =>
        if (sg)
          if (si.check > 0) { // check is position -> push Input + offset onto stack
            val newStack = stack :+ (inputNumberArray(index) + si.offset).toLong
            (z, newStack, true, index + 1)
          } else // check is negative -> pop, check input is correct value
            stack match {
              case Nil => (z, stack, false, index + 1) // popping from empty stack, fail
              case x =>
                val poppedValue = x.last
                val newStack = x.dropRight(1)
                if (poppedValue + si.check != inputNumberArray(index)) {
                  val newNewStack = newStack :+ (inputNumberArray(index) + si.offset).toLong
                  (z, newNewStack, true, index + 1)
                } else
                  (z, stack, false, index + 1)
            }
        else
          (z, stack, false, index + 1)
    })
    if (success)
      z == 0 && stack.isEmpty
    else false
  }

  println(processNumberSmarter(13579246899999L, infoSections))

//  var x: Boolean = true
//  var number: Long = 97499997991199L
//  //  var number: Long = 98491959997994L
//  while (x) {
//    if (!number.toString.contains("0")) {
//      println(number)
//      val valid = processNumberSmarter(number, infoSections)
//      if (valid) {
//        println("Found a valid")
//        println(number)
//        throw new RuntimeException("Finished")
//      }
//    }
//    number -= 1
//  }

  val index = "ABCDEFGHIJKLMN"

  infoSections.zipWithIndex.foreach({
    case (si, i) =>
      if (si.check > 0)
        println(s"${index(i)} + ${si.offset}")
      else
        println(s"${index(i)} =  - ${si.offset}")
  })

  infoSections.zipWithIndex.foldLeft(List[String]())({
    case (l, (si, i)) =>
      if (si.check > 0)
        l :+ s"input[${i + 1}] + ${si.offset}"
      else {
        println(s"input[${i + 1}] == ${l.last} - ${si.offset}")
        l.dropRight(1)
      }
  })

  /*
  input[3] == input[2] + 5
  input[6] == input[5]
  input[8] == input[7] - 2
  input[10] == input[9] - 8
  input[11] == input[4] - 8
  input[12] == input[1] + 2
  input[13] == input[0]

  ..............
  97499999791199

   */

  // PART 1: 97499997991199 too high

//  println(processNumberSmarter(97499997991199L, infoSections))
//  println(processNumber(97499997991199L, inputInstructions))

//  inp w
//  mul x 0
//  add x z
//  mod x 26
//  div z {DIV}
//  add x {CHECK}
//  eql x w
//  eql x 0
//  mul y 0
//  add y 25
//  mul y x
//  add y 1
//  mul z y
//  mul y 0
//  add y w
//  add y {OFFSET}
//  mul y x
//  add z y

}
