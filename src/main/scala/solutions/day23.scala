//package solutions
//
//import utils.IO._
//
//object day23 extends App {
//
//  val A = "A"
//  val B = "B"
//  val C = "C"
//  val D = "D"
//
//  val caveSystem = readFile(23)
//
//  val emptySpace = (1 to 11).map(_ => ".").toVector
//
//  case class Cave(
//    space: Vector[String],
//    caveA: List[String],
//    caveB: List[String],
//    caveC: List[String],
//    caveD: List[String],
//    energyUsed:Int = 0
//  ) {
//    def moveOutside(cave: String, index: Int):Cave = {
//      cave match {
//        case A => Cave(space.updated(index, caveA.last), caveA.dropRight(1), caveB, caveC, caveD, energyUsed+(2-caveA.dropRight(1).length)+math.abs(2-index))
//        case B => Cave(space.updated(index, caveB.last), caveA, caveB.dropRight(1), caveC, caveD, energyUsed+(2-caveB.dropRight(1).length)+math.abs(4-index))
//        case C => Cave(space.updated(index, caveC.last), caveA, caveB, caveC.dropRight(1), caveD, energyUsed+(2-caveC.dropRight(1).length)+math.abs(6-index))
//        case D => Cave(space.updated(index, caveD.last), caveA, caveB, caveC, caveD.dropRight(1), energyUsed+(2-caveD.dropRight(1).length)+math.abs(8-index))
//      }
//    }
//    def moveInside(letter: String, fromIndex:Int):Cave = {
//      val newSpace = space.updated(fromIndex, ".")
//      letter match {
//        case A if caveA.isEmpty => Cave(newSpace, List(A), caveB, caveC, caveD, energyUsed+math.abs(2-fromIndex)+2)
//        case A if caveA.nonEmpty => Cave(newSpace, caveA:+A, caveB, caveC, caveD, energyUsed+math.abs(2-fromIndex)+1)
//        case B if caveB.isEmpty => Cave(newSpace, caveA, List(B), caveC, caveD, energyUsed+math.abs(4-fromIndex)+2)
//        case B if caveB.nonEmpty => Cave(newSpace, caveA, caveB:+B, caveC, caveD, energyUsed+math.abs(4-fromIndex)+1)
//        case C if caveC.isEmpty => Cave(newSpace,caveA, caveB, List(C), caveD, energyUsed+math.abs(6-fromIndex)+2)
//        case C if caveC.nonEmpty => Cave(newSpace, caveA, caveB, caveC:+C, caveD, energyUsed+math.abs(6-fromIndex)+1)
//        case D if caveD.isEmpty => Cave(newSpace, caveA, caveB, caveC, List(D), energyUsed+math.abs(8-fromIndex)+2)
//        case D if caveD.nonEmpty => Cave(newSpace, caveA, caveB, caveC, caveD:+D, energyUsed+math.abs(8-fromIndex)+1)
//      }
//    }
//    def availableMoves() = {
//      space match {
//        case emptySpace => List(0, 1, 3, 5, 7, 9, 10).flatMap(i => List(
//          moveOutside(A,i),
//          moveOutside(B,i),
//          moveOutside(C,i),
//          moveOutside(D,i)
//        ))
//        case
//      }
//    }
//  }
//
//  val caveA = List(B, D)
//  val caveB = List(A, D)
//  val caveC = List(B, C)
//  val caveD = List(A, C)
//
//  val part1StartCave = Cave(emptySpace, List(B, D),List(A, D),List(B, C),List(A, C))
//
//}
//
////C3
////B8
////C3
////C4
////A3
////D7
////D8
////A5
////B6
////A3
////B5
////A8
////7000+8000+300+300+400+60+50+5+3+8
////
////D6
////D8
////C6
////C8
////B5
////A6
////B3
////A9
////B5
////C7
////C7
////D3
////D3
////
////9+6+50+50+600+800+700+700+6000+8000+3000+3000
