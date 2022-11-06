package com.leetcode.cosminci._2500

import scala.util.chaining.*

object _2461_MaxSumOfDistinctSubarraysWithLenK:

  def maximumSubarraySum(nums: Array[Int], k: Int): Long =
    def dfs(l: Int, r: Int, sum: Long, seen: Set[Int]): Long =
      if r == nums.length then 0L
      else Iterator
        .iterate((l, sum, seen)) { case (l, sum, seen) => (l + 1, sum - nums(l), seen - nums(l)) }
        .dropWhile { case (_, _, seen) => seen.contains(nums(r)) }
        .next()
        .pipe { case (l, sum, seen) =>
          val (newSum, newSeen, newL) =
            if r - l + 1 <= k then (sum + nums(r), seen + nums(r), l)
            else (sum + nums(r) - nums(l), seen + nums(r) - nums(l), l + 1)
          if r - newL + 1 < k then dfs(newL, r + 1, newSum, newSeen)
          else newSum.max(dfs(newL, r + 1, newSum, newSeen))
        }

    dfs(l = 0, r = 0, sum = 0L, seen = Set.empty)
