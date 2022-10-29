package com.leetcode.cosminci._1000

object _930_BinarySubarraysWithSum {
  def numSubarraysWithSum(nums: Array[Int], goal: Int): Int =
    nums.foldLeft(0, 0, Map(0 -> 1)) {
      case ((result, prefixSum, prevSums), n) =>
        val newPrefixSum = prefixSum + n
        val newResult = result + prevSums.getOrElse(newPrefixSum - goal, 0)
        (newResult, newPrefixSum, prevSums.updated(newPrefixSum, prevSums.getOrElse(newPrefixSum, 0) + 1))
    }._1
}
