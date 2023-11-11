package com.leetcode.cosminci._3000

import scala.collection.mutable
import scala.util.chaining.*

object _2915_LengthOfLongestSubseqThatSumsToTarget:

  def lengthOfLongestSubsequence(nums: List[Int], target: Int): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, target: Int): Int = mem.getOrElseUpdate((i, target),
      if target == 0 then 0
      else if target < 0 || i >= nums.length then -1001
      else (1 + dfs(i + 1, target - nums(i))).max(dfs(i + 1, target))
    )
    dfs(i = 0, target).pipe(res => if res > 0 then res else -1)
