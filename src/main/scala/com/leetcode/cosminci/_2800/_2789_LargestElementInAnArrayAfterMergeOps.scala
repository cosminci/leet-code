package com.leetcode.cosminci._2800

import scala.util.chaining.*

object _2789_LargestElementInAnArrayAfterMergeOps:

  def maxArrayValue(nums: Array[Int]): Long =
    nums.foldRight((0L, 0L)) { case (curr, (res, next)) =>
      if curr > next then (res.max(curr), curr)
      else (res.max(curr + next), curr + next)
    }.pipe { case (res, _) => res }
