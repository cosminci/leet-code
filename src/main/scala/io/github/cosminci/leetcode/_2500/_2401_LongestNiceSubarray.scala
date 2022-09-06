package io.github.cosminci.leetcode._2500

object _2401_LongestNiceSubarray:

  def longestNiceSubarray(nums: Array[Int]): Int =
    nums.indices
      .foldLeft(0, 0, 0) { case ((left, bitmask, max), right) =>
        val (newLeft, newBitmask) = Iterator
          .iterate((left, bitmask)) { case (left, bitmask) => (left + 1, bitmask ^ nums(left)) }
          .dropWhile { case (_, bitmask) => (bitmask & nums(right)) != 0 }
          .next()
        (newLeft, newBitmask | nums(right), max.max(right - newLeft + 1))
      }._3
