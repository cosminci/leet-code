package com.leetcode.cosminci._1600

import scala.util.chaining.*

object _1539_KthMissingPositiveNumber:

  def findKthPositive(arr: Array[Int], k: Int): Int =
    Iterator
      .iterate((0, arr.length)) { case (l, r) =>
        val mid = l + (r - l) / 2
        if arr(mid) - mid - 1 < k then (mid + 1, r) else (l, mid)
      }
      .dropWhile { case (l, r) => l < r }.next()
      .pipe { case (l, _) => l + k }
