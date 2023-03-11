package com.leetcode.cosminci._2600

import scala.collection.mutable

object _2585_NumWaysToEarnPoints:

  def waysToReachTarget(target: Int, types: Array[Array[Int]]): Int =
    val mem = mutable.Map.empty[(Int, Int), Long]
    def dfs(i: Int, target: Int): Long = mem.getOrElseUpdate((i, target),
      if target == 0 then 1
      else if i == types.length || target < 0 then 0
      else (0 to types(i)(0)).foldLeft(0L) { (sum, j) =>
        (sum + dfs(i + 1, target - j * types(i)(1))) % 1_000_000_007
      })
    dfs(i = 0, target).toInt
