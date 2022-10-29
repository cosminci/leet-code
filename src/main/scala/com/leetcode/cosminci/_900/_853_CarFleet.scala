package com.leetcode.cosminci._900

object _853_CarFleet:
  def main(args: Array[String]): Unit =
    println(carFleet(12, Array(10, 8, 0, 5, 3), Array(2, 4, 1, 1, 3)))

  def carFleet(target: Int, positions: Array[Int], speeds: Array[Int]): Int =
    positions.map(_.toDouble).zip(speeds)
      .sortBy { case (p, s) => -p }
      .foldLeft(0, 0.0) {
        case ((fleetCount, prevTimeToTarget), (position, speed)) =>
          val timeToTarget = (target - position) / speed
          Option
            .when(timeToTarget > prevTimeToTarget)(fleetCount + 1, timeToTarget)
            .getOrElse(fleetCount, prevTimeToTarget)
      }._1
