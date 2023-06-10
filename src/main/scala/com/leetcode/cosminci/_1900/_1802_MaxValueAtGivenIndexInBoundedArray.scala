package com.leetcode.cosminci._1900

import scala.util.chaining.*

object _1802_MaxValueAtGivenIndexInBoundedArray:

  def maxValue(n: Int, index: Int, maxSum: Int): Int =
    val j = n - index - 1
    Iterator
      .iterate((0L, maxSum.toLong)) { case (l, r) =>
        val mid   = (l + r + 1) / 2
        val need1 = Option.when(mid > index)((mid - index - 1) * (mid - index)).getOrElse(0L)
        val need2 = Option.when(mid > j)((mid - j - 1) * (mid - j)).getOrElse(0L)
        val need  = mid * mid - (need1 + need2) / 2
        if need <= maxSum - n then (mid, r) else (l, mid - 1)
      }
      .dropWhile { case (l, r) => l < r }.next()
      .pipe { case (l, _) => (l + 1).toInt }
