package com.leetcode.cosminci._3100

object _3010_DivideAnArrayIntoSubarraysWithMinCostI:

  def minimumCost(nums: Array[Int]): Int =
    nums.head + nums.tail.sorted.take(2).sum
