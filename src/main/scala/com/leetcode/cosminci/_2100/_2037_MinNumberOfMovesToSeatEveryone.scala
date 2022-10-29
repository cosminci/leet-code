package com.leetcode.cosminci._2100

object _2037_MinNumberOfMovesToSeatEveryone:
  def main(args: Array[String]): Unit =
    println(minMovesToSeat(Array(2, 2, 6, 6), Array(1, 3, 2, 6)))
    println(minMovesToSeat(Array(3, 1, 5), Array(2, 7, 4)))
    println(minMovesToSeat(Array(4, 1, 5, 9), Array(1, 3, 2, 6)))

  def minMovesToSeat(seats: Array[Int], students: Array[Int]): Int =
    seats.sorted.zip(students.sorted).foldLeft(0) { case (moves, (seat, student)) =>
      moves + math.abs(seat - student)
    }
