package com.leetcode.cosminci._700

object _673_NumberOfLongestIncreasingSubsequence:

  def findNumberOfLIS(nums: Array[Int]): Int =
    val lengths = Array.fill(nums.length)(1)
    val counts  = Array.fill(nums.length)(1)

    for
      end   <- 1 until nums.length
      start <- 0 until end
      if nums(end) > nums(start)
    do
      if lengths(start) + 1 > lengths(end) then
        lengths(end) = lengths(start) + 1
        counts(end) = counts(start)
      else if lengths(start) + 1 == lengths(end) then counts(end) += counts(start)

    lengths.indices.filter(i => lengths(i) == lengths.max).map(counts).sum
