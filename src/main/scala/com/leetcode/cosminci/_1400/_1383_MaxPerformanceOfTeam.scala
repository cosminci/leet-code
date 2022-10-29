package com.leetcode.cosminci._1400

import scala.collection.immutable.TreeSet

object _1383_MaxPerformanceOfTeam:

  def maxPerformance(n: Int, speed: Array[Int], efficiency: Array[Int], k: Int): Int = {
    efficiency
      .zip(speed)
      .zipWithIndex
      .sorted
      .foldRight(TreeSet.empty[(Int, Int)], 0L, 0L) { case (((eff, spd), idx), (topSpeeds, speedSum, res)) =>
        val (newTopSpeeds, newSpeedSum) = (topSpeeds + ((spd, idx)), speedSum + spd)
        if newTopSpeeds.size <= k then (newTopSpeeds, newSpeedSum, res.max(newSpeedSum * eff))
        else
          val (slowest, remaining) = (newTopSpeeds.head._1, newTopSpeeds.tail)
          (remaining, newSpeedSum - slowest, res.max((newSpeedSum - slowest) * eff))
      }
      ._3 % 1_000_000_007
  }.toInt
