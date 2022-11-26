package com.leetcode.cosminci._1300

import scala.collection.immutable.TreeMap
import scala.util.chaining.*

object _1235_MaxProfitInJobScheduling:

  def jobScheduling(startTime: Array[Int], endTime: Array[Int], profit: Array[Int]): Int =
    startTime
      .zip(endTime)
      .zip(profit)
      .map { case ((start, end), profit) => (start, end, profit) }
      .sortBy { case (_, end, _) => end }
      .foldLeft(TreeMap(0 -> 0)) { case (dp, (start, end, profit)) =>
        val newProfit = dp.maxBefore(start + 1).map { case (_, profit) => profit }.getOrElse(0) + profit
        if newProfit <= dp.last.pipe { case (_, maxProfit) => maxProfit } then dp else dp + (end -> newProfit)
      }
      .last
      .pipe { case (_, maxProfit) => maxProfit }
