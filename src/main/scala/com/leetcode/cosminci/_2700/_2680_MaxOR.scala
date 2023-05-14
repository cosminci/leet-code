package com.leetcode.cosminci._2700

object _2680_MaxOR:

  def maximumOr(nums: Array[Int], k: Int): Long =
    val prefixOr = nums.scanLeft(0L)(_ | _)
    val suffixOr = nums.scanRight(0L)(_ | _)
    nums.indices.map(i => prefixOr(i) | (nums(i).toLong << k) | suffixOr(i + 1)).max
