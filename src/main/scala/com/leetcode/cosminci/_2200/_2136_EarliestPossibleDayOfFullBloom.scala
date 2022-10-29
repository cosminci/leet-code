package com.leetcode.cosminci._2200

object _2136_EarliestPossibleDayOfFullBloom:
  def main(args: Array[String]): Unit =
    println(earliestFullBloom(Array(1, 4, 3), Array(2, 3, 1)))

  def earliestFullBloom(plantTime: Array[Int], growTime: Array[Int]): Int =
    plantTime
      .zip(growTime)
      .sortBy { case (_, gTime) => -gTime }
      .foldLeft(0, 0) { case ((currTime, maxBloomTime), (pTime, gTime)) =>
        val timeAfterPlanting = currTime + pTime
        (timeAfterPlanting, maxBloomTime.max(timeAfterPlanting + gTime))
      }
      ._2
