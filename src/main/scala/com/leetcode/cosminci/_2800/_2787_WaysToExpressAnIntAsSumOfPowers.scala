package com.leetcode.cosminci._2800

import scala.collection.mutable

object _2787_WaysToExpressAnIntAsSumOfPowers:

  def numberOfWays(n: Int, x: Int): Int =
    val nums = Iterator.iterate(1)(_ + 1).map(math.pow(_, x).toInt).takeWhile(_ <= n).toList

    val mem = mutable.Map.empty[(Int, Int), Long]
    def dfs(i: Int, target: Int): Long = mem.getOrElseUpdate((i, target),
      if target == 0 then 1
      else if i == nums.length then 0
      else if nums(i) > target then 0
      else (dfs(i + 1, target - nums(i)) + dfs(i + 1, target)) % 1_000_000_007
    )

    dfs(i = 0, target = n).toInt
