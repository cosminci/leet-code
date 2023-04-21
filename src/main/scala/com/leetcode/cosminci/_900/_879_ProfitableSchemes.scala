package com.leetcode.cosminci._900

import scala.collection.mutable

object _879_ProfitableSchemes:

  def profitableSchemes(n: Int, minProfit: Int, groups: Array[Int], profits: Array[Int]): Int =
    val mod = 1_000_000_007
    val mem = mutable.Map.empty[(Int, Int, Int), Long]

    def dfs(i: Int, m: Int, p: Int): Long = mem.getOrElseUpdate((m, p, i),
      if m == -1 then 0
      else if i == groups.length then if p == 0 then 1 else 0
      else (dfs(i + 1, (m - groups(i)).max(-1), (p - profits(i)).max(0)) + dfs(i + 1, m, p)) % mod
    )

    dfs(i = 0, m = n, p = minProfit).toInt
