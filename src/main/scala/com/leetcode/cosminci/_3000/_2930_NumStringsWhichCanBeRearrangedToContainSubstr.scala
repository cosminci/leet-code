package com.leetcode.cosminci._3000

import scala.collection.mutable

object _2930_NumStringsWhichCanBeRearrangedToContainSubstr:

  def stringCount(n: Int): Int =
    val (l, e1, e2, t) = (1 << 3, 1 << 2, 1 << 1, 1)
    val leet           = l | e1 | e2 | t

    val mem = mutable.Map.empty[(Int, Int), Long]
    def dfs(i: Int, mask: Int): Long = mem.getOrElseUpdate((i, mask),
      if i == n then if mask == leet then 1 else 0
      else
        val waysLT    = dfs(i + 1, mask | l) + dfs(i + 1, mask | t)
        val waysE     = if (mask & e2) > 0 then dfs(i + 1, mask | e1) else dfs(i + 1, mask | e2)
        val waysOther = 23 * dfs(i + 1, mask)
        (waysLT + waysE + waysOther) % 1_000_000_007
    )

    dfs(i = 0, mask = 0).toInt
