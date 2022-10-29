package com.leetcode.cosminci._2500

object _2453_DestroySequentialTargets:

  def destroyTargets(nums: Array[Int], space: Int): Int =
    nums.sorted
      .groupBy(_ % space)
      .values
      .maxBy(nums => (nums.length, -nums.head))
      .head
