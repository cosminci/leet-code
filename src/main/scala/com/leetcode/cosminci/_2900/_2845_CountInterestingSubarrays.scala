package com.leetcode.cosminci._2900

object _2845_CountInterestingSubarrays:

  def countInterestingSubarrays(nums: List[Int], mod: Int, k: Int): Long =
    nums
      .foldLeft(Map(0 -> 1), 0, 0L) { case ((prefixCount, prefixSum, result), n) =>
        val newPrefixSum   = (prefixSum + (if n % mod == k then 1 else 0)) % mod
        val newResult      = result + prefixCount.getOrElse((newPrefixSum - k + mod) % mod, 0)
        val newPrefixCount = prefixCount.updated(newPrefixSum, prefixCount.getOrElse(newPrefixSum, 0) + 1)
        (newPrefixCount, newPrefixSum, newResult)
      }._3
