package com.leetcode.cosminci._3000

object _2914_MinChangesToMakeBinaryStrBeautiful:

  def minChanges(s: String): Int =
    (0 until s.length by 2)
      .map(i => if s(i) == s(i + 1) then 0 else 1)
      .sum
