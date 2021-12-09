package solutions

import utils.IO.{readFile, readInts}

object day9 extends App {

  val caveFloor = readFile(9).map(_.split("").toList.map(_.toInt))

  val testFloor = List(
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
  ).map(_.split("").toList.map(_.toInt))

  def safeLookup(value: List[List[Int]], tuple: (Int, Int)): Option[Int] = {
    if (0 <= tuple._2 && tuple._2 < value.length)
      if (0 <= tuple._1 && tuple._1 < value(tuple._2).length)
        return Some(value(tuple._2)(tuple._1))
    None
  }

  def getSurrounding(coord: (Int, Int)): List[(Int, Int)] =
    List((1, 0), (0, 1), (-1, 0), (0, -1)).map({
      case (x, y) => (coord._1 + x, coord._2 + y)
    })

  def checkAdjacent(ll: List[List[Int]], coord: (Int, Int)): Option[Int] = {

    val currentValue = ll(coord._2)(coord._1)

    val coords = getSurrounding(coord)
    val surroundingVals: List[Option[Int]] = coords.map(c => safeLookup(ll, c))
    if (
      surroundingVals.forall({
        case Some(x) => x > currentValue
        case None    => true
      })
    )
      Some(currentValue)
    else None
  }

  def findLowPoints(floor: List[List[Int]]) = {
    val comp = for {
      j <- floor.indices
      i <- floor.head.indices
    } yield ((i, j), checkAdjacent(floor, (i, j)))

    val (lowPointCoords, lowPointValues) = comp.filter(_._2.nonEmpty).unzip

    println(s"Found ${lowPointValues.count(_.nonEmpty)} lowPoints")

    val riskLevel = lowPointValues.foldLeft(0)({
      case (cumSum, Some(x)) => cumSum + x + 1
      case (cumSum, None)    => cumSum
    })
    (lowPointCoords, riskLevel)
  }

  def getNonNineNeighbours(ll: List[List[Int]], coord: (Int, Int), basin: List[(Int, Int)]): List[(Int, Int)] = {
    if (basin.contains(coord)) println("Ive been here before....")

    val tempBasin =
      if (!basin.contains(coord))
        coord :: basin
      else basin

    val coords = getSurrounding(coord)
    val surroundingVals = coords.map(c => (safeLookup(ll, c), c))

    val toVisit = surroundingVals
      .filter(
        _._1.nonEmpty // this is the case where the coord is off the map
      )
      .filter(
        !_._1.contains(9) // this is where we've hit the edge of the basin
      )
      .filterNot({
        case (a, b) => tempBasin.contains((a, b)) // this is where we've already been to the proposed coordinate
      })

    val newBasin: List[(Int, Int)] = toVisit.foldLeft(tempBasin)({
      case (b, (Some(x), (i, j))) =>
        if (b.contains((i, j)))
          b
        else
          getNonNineNeighbours(ll, (i, j), b)
    })
    newBasin
  }

  def findBasins(floor: List[List[Int]], lowPoints: List[(Int, Int)]) = {
    val basins = lowPoints.map({
      case (i, j) => getNonNineNeighbours(floor, (i, j), List.empty[(Int, Int)])
    })
    basins
  }

  val lowPoints = findLowPoints(caveFloor)

  println(s"part1: ${lowPoints._2}")
  val basins = findBasins(caveFloor, lowPoints._1.toList)

  val areas = basins.sortBy(_.length).takeRight(3).foldLeft(1)((i, l) => i * l.length)
  println(s"part2: $areas")

}
