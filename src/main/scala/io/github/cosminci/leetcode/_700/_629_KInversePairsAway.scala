package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _629_KInversePairsAway:

  def kInversePairs(n: Int, k: Int): Int =
    val mod = 1_000_000_007L
    val mem = mutable.Map.empty[(Int, Int), Long]

    def dfs(n: Int, k: Int): Long = mem.getOrElseUpdate((n, k),
      if n == 0 then 0
      else if k == 0 then 1
      else
        val toSubtract = if k >= n then dfs(n - 1, k - n) else 0
        (dfs(n, k - 1) + dfs(n - 1, k) - toSubtract + mod) % mod
    )

    val toSubtract = if k > 0 then dfs(n, k - 1) else 0
    ((dfs(n, k) - toSubtract + mod) % mod).toInt
