package com.leetcode.cosminci._3000

import scala.collection.mutable

object _2919_MinIncrementOpsToMakeArrayBeautiful:

  def minIncrementOperations(nums: Array[Int], k: Int): Long =
    val mem = mutable.Map.empty[Int, Long]
    def dfs(i: Int): Long = mem.getOrElseUpdate(i,
      if i > nums.length - 3 then 0L
      else Seq(0, 1, 2).map(di => 0.max(k - nums(i + di)) + dfs(i + di + 1)).min
    )

    dfs(i = 0)
