package com.leetcode.cosminci._3100

object _3065_MinOpsToExceedThresholdValueI:

  def minOperations(nums: Array[Int], k: Int): Int =
    nums.count(_ < k)
