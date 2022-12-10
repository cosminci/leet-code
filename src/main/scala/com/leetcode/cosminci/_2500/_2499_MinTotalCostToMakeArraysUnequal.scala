package com.leetcode.cosminci._2500

import scala.util.chaining.*

object _2499_MinTotalCostToMakeArraysUnequal:

  def minimumTotalCost(nums1: Array[Int], nums2: Array[Int]): Long =
    val zipped = nums1.zip(nums2).zipWithIndex

    val (freq, cost, total) = zipped
      .foldLeft(Map.empty[Int, Int].withDefaultValue(0), 0L, 0) { case ((freq, cost, total), ((x, y), i)) =>
        if x != y then (freq, cost, total)
        else (freq.updated(x, freq(x) + 1), cost + i, total + 1)
      }
    if total == 0 then return 0

    val (maxFreqKey, maxFreq) = freq.maxBy { case (_, freq) => freq }

    zipped
      .foldLeft(cost, total) { case ((cost, total), ((x, y), i)) =>
        if 2 * maxFreq <= total then return cost
        else if Seq(x, y, maxFreqKey).distinct.size != 3 then (cost, total)
        else (cost + i, total + 1)
      }
      .pipe { case (cost, total) => if 2 * maxFreq <= total then cost else -1 }
