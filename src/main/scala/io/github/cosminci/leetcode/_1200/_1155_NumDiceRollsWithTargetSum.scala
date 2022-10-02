package io.github.cosminci.leetcode._1200

import scala.collection.mutable

object _1155_NumDiceRollsWithTargetSum:

  def numRollsToTarget(n: Int, k: Int, target: Int): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(n: Int, target: Int): Int = mem.getOrElseUpdate((n, target),
      if target < 0 then 0
      else if n == 0 then if target == 0 then 1 else 0
      else (1 to k).foldLeft(0)((sum, opt) => (sum + dfs(n - 1, target - opt)) % 1_000_000_007)
    )

    dfs(n, target)
