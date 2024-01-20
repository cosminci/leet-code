package com.leetcode.cosminci._3000

object _2980_CheckIfBitwiseORHasTrailingZeros:

  def hasTrailingZeros(nums: Array[Int]): Boolean =
    nums.count(_ % 2 == 0) > 1
