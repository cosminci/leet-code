package com.leetcode.cosminci._2400

object _2348_NumZeroFilledSubarrays:

  def zeroFilledSubarray(nums: Array[Int]): Long =
    (nums :+ 1).foldLeft(0L, 0L) {
      case ((numSubArrays, zeroStreak), n) =>
        if (n == 0) (numSubArrays, zeroStreak + 1)
        else (numSubArrays + zeroStreak * (zeroStreak + 1) / 2, 0)
      }._1
