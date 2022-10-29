package com.leetcode.cosminci._2400

object _2317_MaxXORAfterOps:

  def maximumXOR(nums: Array[Int]): Int =
    nums.reduce(_ | _)
