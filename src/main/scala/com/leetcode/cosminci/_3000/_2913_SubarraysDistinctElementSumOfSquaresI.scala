package com.leetcode.cosminci._3000

object _2913_SubarraysDistinctElementSumOfSquaresI:

  def sumCounts(nums: List[Int]): Int =
    nums.indices.foldLeft(0) { (result, i) =>
      (i to nums.length).foldLeft(result) { (result, j) =>
        val c = nums.slice(i, j).distinct.size
        result + c * c
      }
    }
