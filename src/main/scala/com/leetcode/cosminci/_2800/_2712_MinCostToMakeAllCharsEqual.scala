package com.leetcode.cosminci._2800

object _2712_MinCostToMakeAllCharsEqual:

  def minimumCost(s: String): Long =
    (1 until s.length)
      .filter(i => s(i) != s(i - 1))
      .map(i => i.min(s.length - i).toLong)
      .sum
