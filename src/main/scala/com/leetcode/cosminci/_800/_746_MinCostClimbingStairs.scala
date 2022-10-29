package com.leetcode.cosminci._800

import scala.collection.mutable

object _746_MinCostClimbingStairs {

  def minCostClimbingStairs(cost: Array[Int]): Int = {
    val mem = mutable.Map.empty[Int, Int]

    def dfs(i: Int): Int = mem.getOrElseUpdate(i,
      if (i >= cost.length) 0
      else cost(i) + dfs(i + 1).min(dfs(i + 2))
    )

    dfs(i = 0).min(dfs(i = 1))
  }
}
