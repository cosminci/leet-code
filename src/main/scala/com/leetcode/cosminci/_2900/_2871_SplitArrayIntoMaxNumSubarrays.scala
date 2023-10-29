package com.leetcode.cosminci._2900

import scala.util.chaining.*

object _2871_SplitArrayIntoMaxNumSubarrays:

  def maxSubarrays(nums: Array[Int]): Int =
    nums
      .foldLeft(0, 0) { case ((res, curr), n) =>
        val newCurr = if curr == 0 then n else curr & n
        val newRes  = if newCurr == 0 then res + 1 else res
        (newRes, newCurr)
      }
      .pipe { case (res, _) => res.max(1) }
