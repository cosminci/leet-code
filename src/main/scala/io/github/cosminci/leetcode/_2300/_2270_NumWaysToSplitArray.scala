package io.github.cosminci.leetcode._2300

object _2270_NumWaysToSplitArray:

  def waysToSplitArray(nums: Array[Int]): Int =
    val prefixSum = nums.scanLeft(0L)(_ + _).slice(1, nums.length)
    val suffixSum = nums.scanRight(0L)(_ + _).slice(1, nums.length)
    prefixSum.zip(suffixSum).count { case (left, right) => left >= right }
