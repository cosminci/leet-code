package com.leetcode.cosminci._2200

object _2104_SumOfSubarrayRanges:
  def subArrayRanges(nums: Array[Int]): Long =
    nums.indices.foldLeft(0L) { (sum, i) =>
      (i until nums.length).foldLeft(sum, nums(i), nums(i)) {
        case ((sum, min, max), j) =>
          val currMin = nums(j).min(min)
          val currMax = nums(j).max(max)
          (sum + currMax - currMin, currMin, currMax)
        }._1
    }
