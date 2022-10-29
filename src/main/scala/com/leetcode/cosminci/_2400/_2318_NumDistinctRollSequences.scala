package com.leetcode.cosminci._2400

import com.leetcode.cosminci.utils.gcd

import scala.collection.mutable

object _2318_NumDistinctRollSequences:

  def distinctSequences(n: Int): Int =
    val mem = mutable.Map.empty[(Int, Int, Int), Long]
    def dfs(n: Int, p1: Int, p2: Int): Long = mem.getOrElseUpdate((n, p1, p2),
      if n == 0 then 1
      else (1 to 6)
        .filter(r => r != p1 && r != p2 && (p1 == 0 || gcd(r, p1) == 1))
        .map(r => dfs(n - 1, r, p1))
        .reduce((a, b) => (a + b) % 1_000_000_007)
    )

    dfs(n, p1 = 0, p2 = 0).toInt
