package com.leetcode.cosminci._2800

import scala.util.chaining.*

object _2765_LongestAlternatingSubarray:

  def alternatingSubarray(nums: Array[Int]): Int =
    (1 until nums.length)
      .foldLeft(-1, -1) { case ((maxLen, currLen), i) =>
        val newLen =
          if currLen > 0 && nums(i) == nums(i - 2) then currLen + 1
          else if nums(i) == nums(i - 1) + 1 then 2
          else -1
        (maxLen.max(newLen), newLen)
      }
      .pipe { case (res, _) => res }
