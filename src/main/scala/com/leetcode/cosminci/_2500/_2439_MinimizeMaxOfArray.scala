package com.leetcode.cosminci._2500

object _2439_MinimizeMaxOfArray:

  def minimizeArrayValue(nums: Array[Int]): Int =
    nums
      .scanLeft(0L)(_ + _)
      .tail
      .zipWithIndex
      .foldLeft(0L) { case (res, (sum, i)) => res.max((sum + i) / (i + 1)) }
      .toInt
