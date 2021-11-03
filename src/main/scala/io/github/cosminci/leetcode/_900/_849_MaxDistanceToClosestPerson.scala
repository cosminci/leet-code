package io.github.cosminci.leetcode._900

object _849_MaxDistanceToClosestPerson:
  def main(args: Array[String]): Unit =
    println(maxDistToClosest(Array(1, 0, 0, 0, 1, 0, 1)))
    println(maxDistToClosest(Array(1, 0, 0, 0)))
    println(maxDistToClosest(Array(0, 1)))

  def maxDistToClosest(seats: Array[Int]): Int =
    val maxLeft = seats.scanLeft(seats.length) { (distanceToLeft, seat) =>
      if seat == 0 then distanceToLeft + 1 else 0
    }.drop(1)

    val maxRight = seats.scanRight(seats.length) { (seat, distanceToRight) =>
      if seat == 0 then distanceToRight + 1 else 0
    }.dropRight(1)

    maxLeft.zip(maxRight).map((x, y) => x.min(y)).max
