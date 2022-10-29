package com.leetcode.cosminci._1000

object _918_MaxSumCircularSubarray {
  def maxSubarraySumCircular(nums: Array[Int]): Int = {
    val (globalMax, _, globalMin, _) = nums.foldLeft(nums.head, 0, nums.head, 0) {
      case ((globalMax, localMax, globalMin, localMin), n) =>
        val newLocalMax = n.max(localMax + n)
        val newLocalMin = n.min(localMin + n)
        (globalMax.max(newLocalMax), newLocalMax, globalMin.min(newLocalMin), newLocalMin)
    }
    Option.when(globalMax > 0)(globalMax.max(nums.sum - globalMin)).getOrElse(globalMax)
  }
}
