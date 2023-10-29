package com.leetcode.cosminci._2900

object _2874_MaxValueOfOrderedTripletII:

  def maximumTripletValue(nums: Array[Int]): Long =
    nums
      .foldLeft(0L, 0L, 0L) { case ((result, maxDiff, maxNum), n) =>
        val newRes = result.max(maxDiff * n)
        val newMaxDiff = maxDiff.max(maxNum - n)
        val newMaxNum = maxNum.max(n)
        (newRes, newMaxDiff, newMaxNum)
      }._1
