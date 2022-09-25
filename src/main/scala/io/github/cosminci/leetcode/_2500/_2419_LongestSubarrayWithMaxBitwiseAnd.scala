package io.github.cosminci.leetcode._2500

object _2419_LongestSubarrayWithMaxBitwiseAnd:

  def longestSubarray(nums: Array[Int]): Int =
    val max = nums.max
    nums
      .foldLeft(0, 0) { case ((maxLen, currLen), n) =>
        if n != max then (maxLen, 0)
        else (maxLen.max(currLen + 1), currLen + 1)
      }._1
