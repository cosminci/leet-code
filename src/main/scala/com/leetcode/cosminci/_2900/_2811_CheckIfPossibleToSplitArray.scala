package com.leetcode.cosminci._2900

object _2811_CheckIfPossibleToSplitArray:

  def canSplitArray(nums: List[Int], m: Int): Boolean =
    nums.length <= 2 || nums.sliding(2).exists(_.sum >= m)
