package com.leetcode.cosminci._2800

import scala.collection.mutable
import scala.util.chaining.*

object _2770_MaxJumpsToReachLastIndex:

  def maximumJumps(nums: Array[Int], target: Int): Int =
    val mem = mutable.Map.empty[Int, Int]
    def dfs(i: Int): Int = mem.getOrElseUpdate(i,
      if i == nums.length - 1 then 0
      else (i + 1 until nums.length)
        .filter(j => (nums(j) - nums(i)).abs <= target)
        .map(j => 1 + dfs(j))
        .maxOption
        .getOrElse(Int.MinValue)
    )

    dfs(i = 0).pipe(r => if r < Int.MinValue / 2 then -1 else r)
