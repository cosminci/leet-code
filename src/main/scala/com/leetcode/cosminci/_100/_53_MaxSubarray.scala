package com.leetcode.cosminci._100

object _53_MaxSubarray:
  def maxSubArray(nums: Array[Int]): Int =
    nums.tail.foldLeft(nums.head, nums.head) {
      case ((prev, max), n) =>
        val curr = n.max(prev + n)
        (curr, max.max(curr))
    }._2
