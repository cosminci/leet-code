package com.leetcode.cosminci._3000

object _2932_MaxStrongPairXorI:

  def maximumStrongPairXor(nums: Array[Int]): Int =
    nums
      .combinations(2)
      .collect { case Array(a, b) if (a - b).abs <= a.min(b) => a ^ b }
      .maxOption.getOrElse(0)
