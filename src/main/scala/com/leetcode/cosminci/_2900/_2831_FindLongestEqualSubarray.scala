package com.leetcode.cosminci._2900

import scala.util.chaining.*

object _2831_FindLongestEqualSubarray:

  def longestEqualSubarray(nums: List[Int], k: Int): Int =
    nums.indices
      .foldLeft(0, 0, Map.empty[Int, Int].withDefaultValue(0)) { case ((res, i, counter), j) =>
        val newCounter = counter.updated(nums(j), counter(nums(j)) + 1)
        val newRes     = res.max(newCounter(nums(j)))
        if j - i + 1 - newRes <= k then (newRes, i, newCounter)
        else (newRes, i + 1, newCounter.updated(nums(i), newCounter(nums(i)) - 1))
      }
      .pipe { case (res, _, _) => res }
