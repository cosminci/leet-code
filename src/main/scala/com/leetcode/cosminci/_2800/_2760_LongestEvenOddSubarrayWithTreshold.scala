package com.leetcode.cosminci._2800

import scala.util.chaining.*

object _2760_LongestEvenOddSubarrayWithTreshold:

  def longestAlternatingSubarray(nums: Array[Int], threshold: Int): Int =
    Iterator
      .iterate((0, 0, 0)) { case (maxLen, currLen, i) =>
        if nums(i) > threshold then (maxLen, 0, i + 1)
        else
          val startLen = if nums(i) % 2 == 0 then 1 else 0
          val newLen =
            if currLen == 0 then startLen
            else if nums(i) % 2 == nums(i - 1) % 2 then startLen else currLen + 1
          (maxLen.max(newLen), newLen, i + 1)
      }
      .dropWhile { case (_, _, i) => i < nums.length }.next()
      .pipe { case (maxLen, _, _) => maxLen }
