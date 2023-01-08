package com.leetcode.cosminci._2600

object _2529_MaxCountOfPosAndNegInt:

  def maximumCount(nums: Array[Int]): Int =
    nums.count(_ > 0) max nums.count(_ < 0)
