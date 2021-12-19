package solutions

import utils.IO._

object day19 extends App {

  var count = 0

  type Coord = (Int, Int, Int)
  type Scanner = (Int, Set[Coord])

  def scannerMap(fileName: String): Map[Int, Scanner] = {
    val scanners: List[List[String]] = readFile(fileName).foldLeft(List(List[String]()))({
      case (acc, a) if a.startsWith("---") => acc :+ List(a)
      case (acc, "")                       => acc
      case (acc, a) =>
        val updatedScanner = acc.last :+ a
        acc.dropRight(1) :+ updatedScanner
    })

    val scannerMap: Map[Int, Scanner] = scanners.foldLeft(Map[Int, Scanner]())({
      case (map, Nil) => map + (0 -> (0, List((4, 5, 6)).toSet))
      case (map, number :: coords) =>
        val num = number.split(" ")(2).toInt
        map + (
          num -> (num, coords.map(_.split(",").map(_.toInt)).map(a => (a(0), a(1), a(2))).toSet)
        )
    })
    scannerMap
  }

  val exampleScannerMap: Map[Int, Scanner] = scannerMap(
    "/Users/bradleyking/dev/aoc2021/src/main/scala/input/19.ex.txt"
  )
  val realScannerMap = scannerMap("/Users/bradleyking/dev/aoc2021/src/main/scala/input/19.txt")

//  println(exampleScannerMap)
//  println(realScannerMap)

  val DX = List(1, 1, 1, 1, -1, -1, -1, -1)
  val DY = List(1, 1, -1, -1, 1, 1, -1, -1)
  val DZ = List(1, -1, 1, -1, 1, -1, 1, -1)

  def shift(n: Int, coord: Coord): Coord =
    n match {
      case 0 => (coord._1, coord._2, coord._3)
      case 1 => (coord._3, coord._1, coord._2)
      case 2 => (coord._2, coord._3, coord._1)
    }

  def direction(n: Int): Coord = (DX(n), DY(n), DZ(n))

  def addThreeTuple(a: Coord, b: Coord): Coord = (a._1 + b._1, a._2 + b._2, a._3 + b._3)
  def multiplyThreeTuple(a: Coord, b: Coord): Coord = (a._1 * b._1, a._2 * b._2, a._3 * b._3)

  def subtractThreeTuple(a: Coord, b: Coord): Coord = {
    count += 1
    (a._1 - b._1, a._2 - b._2, a._3 - b._3)
  }

  def orient(n: Int, coord: Coord): Coord = {
    assert((0 to 23).contains(n))
    val magD = n % 8
    val directionV = direction(magD)
    val magS = math.floor(n / 8).toInt
    val shifted = shift(magS, coord)

    val directioned = multiplyThreeTuple(directionV, shifted)
    directioned
  }

  val vv = (1, 2, 3)
  assert(orient(0, vv) == vv)

  def orientations(t: Coord): Seq[Coord] = orientations(t._1, t._2, t._3)

  def orientations(x: Int, y: Int, z: Int): Seq[Coord] =
    Seq(
      (x, y, z),
      (-y, x, z),
      (-x, -y, z),
      (y, -x, z),
      (-x, y, -z),
      (y, x, -z),
      (x, -y, -z),
      (-y, -x, -z),
      (-z, y, x),
      (-z, x, -y),
      (-z, -y, -x),
      (-z, -x, y),
      (z, y, -x),
      (z, x, y),
      (z, -y, x),
      (z, -x, -y),
      (x, -z, y),
      (-y, -z, x),
      (-x, -z, -y),
      (y, -z, -x),
      (x, z, -y),
      (-y, z, -x),
      (-x, z, y),
      (y, z, x)
    )

//  val vvv = (0 to 23).map(x => orient(x, (404, -588, -901))).sorted
//  val www = orientations(404, -588, -901).sorted
//  assert(vvv == www)

  def compareScanners(a: Scanner, b: Scanner): Option[(Set[Coord], Set[Coord], Coord)] = {

    val (_, a_coords: Set[Coord]) = a
    val (_, b_coords) = b

    // set where each element is all coords transformed by each orientation.
    val res = (for {
      ori <- b_coords.toSeq.map(c => orientations(c)).transpose.map(_.toSet).iterator
      p1 <- a_coords.iterator
      p2 <- ori.iterator
      relativePos = subtractThreeTuple(p1, p2)
      intersect = ori.map(p => addThreeTuple(p, relativePos)).intersect(a_coords)
      if intersect.size >= 12
    } yield (ori, intersect, relativePos)).toList

    res.headOption
  }

  //scanner 4      is at -20,-1133,1061
  //        2 must be at 1105,-1205,1229
  //        3 must be at -92,-2380,-20

  def buildCave(scannerMap: Map[Int, Scanner]): (Set[Coord], Map[Int, Coord]) = {

    val relativePositions = Map[Int, Coord](0 -> (0, 0, 0)) //, (1, (68, -1246, -43)))
    val init = scannerMap(0)._2

    val (rp, _, _, beacons) =
      (1 until scannerMap.size).foldLeft((relativePositions, scannerMap, init, init))({
        case ((rp, sc, d, tb), i) =>
          val availableScanners: List[Scanner] = (0 until scannerMap.size)
            .filterNot(i => rp.keys.toList.contains(i))
            .map(i => scannerMap(i))
            .toList //scanners not already with relative positions

          val comparison = for {
            scanner <- availableScanners
            c <- compareScanners((0, d), scanner)
          } yield (scanner, c)

          assert(comparison.nonEmpty || availableScanners.isEmpty)

          val (totalBeacons, updatedRP, discoveredBeacons) = comparison.foldLeft((tb, rp, Set[Coord]()))({
            case ((totalBeacons, rpMap, discovered), comp) =>
              val ((newScannerName, _), (orientatedCoords, _, pos)) = comp
              val discoveredBeacons = discovered ++ orientatedCoords.map(c => addThreeTuple(c, pos))
              val updatedTotalBeacons = totalBeacons ++ discoveredBeacons
              val updatedRP = rpMap + (newScannerName -> pos)
              (updatedTotalBeacons, updatedRP, discoveredBeacons)
          })

          if (comparison.isEmpty) (rp, sc, d, tb)
          else (updatedRP, sc, discoveredBeacons, totalBeacons)

      })

    (beacons, rp)

  }

  def manhattanDistance(a: Coord, b: Coord): Int = math.abs(b._1 - a._1) + math.abs(b._2 - a._2) + math.abs(b._3 - a._3)

  val (beacons, positions) = buildCave(realScannerMap)
  assert(beacons.size == 365 || beacons.size == 79)
  println(beacons.size)
  // <690

  val maxMD = (for {
    i <- positions.values
    j <- positions.values
  } yield manhattanDistance(i, j)).max

  assert(maxMD == 11060)
  println(maxMD)

  println(count)
}
