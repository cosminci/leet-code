package com.leetcode.cosminci._2400

object _2357_MakeArrayZeroBySubtractingEqualAmounts:

  def minimumOperations(nums: Array[Int]): Int =
    nums.distinct.count(_ > 0)
