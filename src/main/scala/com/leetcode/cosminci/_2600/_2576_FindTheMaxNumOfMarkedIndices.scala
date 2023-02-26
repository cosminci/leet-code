package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2576_FindTheMaxNumOfMarkedIndices:

  def maxNumOfMarkedIndices(nums: Array[Int]): Int =
    val (n, sorted) = (nums.length, nums.sorted)
    Iterator
      .iterate((0, n - n / 2)) { case (i, j) => (i + (if 2 * sorted(i) <= sorted(j) then 1 else 0), j + 1) }
      .dropWhile { case (_, j) => j < n }.next()
      .pipe { case (i, _) => i * 2 }
