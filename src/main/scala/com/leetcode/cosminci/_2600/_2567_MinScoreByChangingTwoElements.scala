package com.leetcode.cosminci._2600

object _2567_MinScoreByChangingTwoElements:

  def minimizeSum(nums: Array[Int]): Int =
    val n = nums.length
    nums.sortInPlace()
    (nums(n - 1) - nums(2)).min(nums(n - 3) - nums.head).min(nums(n - 2) - nums(1))
