package com.leetcode.cosminci._200

object _128_LongestConsecutiveSequence:

  def longestConsecutive(nums: Array[Int]): Int =
    val distinct = nums.toSet
    nums.foldLeft(0) { (prevMax, n) =>
      if distinct.contains(n - 1) then prevMax
      else Iterator
        .iterate(1)(_ + 1)
        .dropWhile(i => distinct.contains(n + i))
        .next()
        .max(prevMax)
    }
