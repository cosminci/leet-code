package com.leetcode.cosminci._2800

object _2740_FindValueOfPartition:

  def findValueOfPartition(nums: Array[Int]): Int =
    nums.sorted.sliding(2).map { case Array(a, b) => (a - b).abs }.min
