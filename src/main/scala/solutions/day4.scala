package solutions

import utils.IO.readFile

import scala.annotation.tailrec

object day4 extends App {

  val xxx = readFile(4)

  val yyy = List(
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
    "",
    "22 13 17 11  0",
    "8  2 23  4 24",
    "21  9 14 16  7",
    "6 10  3 18  5",
    "1 12 20 15 19",
    "",
    "3 15  0  2 22",
    "9 18 13 17  5",
    "19  8  7 25 23",
    "20 11 10 24  4",
    "14 21 16 12  6",
    "",
    "14 21 17 24  4",
    "10 16 15  9 19",
    "18  8 23 26 20",
    "22 11 13  6  5",
    "2  0 12  3  7"
  )

  val numbers: List[Int] = xxx.head.split(",").map(_.toInt).toList
  val rest = xxx.tail.filter(_.nonEmpty)

  val winningIndexes = List(
    List(0, 1, 2, 3, 4),
    List(5, 6, 7, 8, 9),
    List(10, 11, 12, 13, 14),
    List(15, 16, 17, 18, 19),
    List(20, 21, 22, 23, 24),
    List(0, 5, 10, 15, 20),
    List(1, 6, 11, 16, 21),
    List(2, 7, 12, 17, 22),
    List(3, 8, 13, 18, 23),
    List(4, 9, 14, 19, 24)
  )

  trait BoardMethods {
    def winner(lastCalledNumber: Int, quiet: Boolean): Boolean
  }

  case class Board(numbers: List[Int], uncalledSum: Int = 0, calledIndexes: List[Int]) extends BoardMethods {

    override def winner(lastCalledNumber: Int, quiet: Boolean = true): Boolean = {
      val bool = winningIndexes.exists(wi => wi.forall(calledIndexes.contains))
      if (bool)
        if (!quiet) {
          println(s"Board won:: $lastCalledNumber * $uncalledSum}")
          println(s"Board won:: ${lastCalledNumber * uncalledSum}")
        }
      bool
    }
  }

  def inputToBoard(ll: List[String]): Board = {
    val ints: List[Int] = ll.map(_.toInt)
    Board(ints, ints.sum, List())
  }

  def callNumber(b: Board, i: Int): Board = {
    val index = b.numbers.indexOf(i)
    if (index >= 0) {
      val newUncalled = b.uncalledSum - i
      val newCalledIndexes = b.calledIndexes :+ index
      //      println(s"Number $i found at position $index : total indexes found: $newCalledIndexes")
      Board(b.numbers, newUncalled, newCalledIndexes)
    } else b
  }

  val boards = rest
    .sliding(5, 5)
    .map { l =>
      val lists = l.mkString(" ").split("\\s+").toList //.grouped(5).toList
      inputToBoard(lists)
    }
    .toList

  def playGameToWin(boards: List[Board], numbers: List[Int], winner: Boolean): Boolean = {

    @tailrec
    def loop(bs: List[Board], numbers: List[Int], winner: Boolean): Boolean = {
      val numberDrawn = numbers.head
      println(s"Drawing number $numberDrawn")
      val newBoards: List[Board] = bs.map(b => callNumber(b, numberDrawn))
      val winnerThisRound = newBoards.exists(b => b.winner(numberDrawn))
      if (winnerThisRound) {
        println("\n\nFirst Board")
        newBoards.find(b => b.winner(numberDrawn)).map(b => b.winner(numberDrawn, quiet = false))
        true
      } else
        loop(newBoards, numbers.tail, winner = false)
    }

    loop(boards, numbers, winner = false)
  }

  playGameToWin(boards, numbers, winner = false)

  def playGameToLose(boards: List[Board], numbers: List[Int], winner: Boolean): Boolean = {

    @tailrec
    def loop(bs: List[Board], numbers: List[Int], winner: Boolean): Boolean = {
      val numberDrawn = numbers.head
      println(s"Drawing number $numberDrawn")
      val newBoards: List[Board] = bs.map(b => callNumber(b, numberDrawn))
      val filtered = newBoards.filterNot(b => b.winner(numberDrawn))
      val winnerThisRound = newBoards.exists(b => b.winner(numberDrawn))
      if (filtered.isEmpty && winnerThisRound) {
        println("\n\nLast Board")
        newBoards.head.winner(numberDrawn, quiet = false)
        true
      } else
        loop(filtered, numbers.tail, winner = false)
    }

    loop(boards, numbers, winner = false)
  }

  playGameToLose(boards, numbers, winner = false)

}
