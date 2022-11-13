package com.leetcode.cosminci._2500

import scala.collection.mutable

object _2466_CountWaysToBuildGoodStrings:

  def countGoodStrings(low: Int, high: Int, zero: Int, one: Int): Int =
    val mem = mutable.Map.empty[Int, Long]
    def dfs(len: Int): Long = mem.getOrElseUpdate(len,
      if len > high then 0
      else
        val localCount = if len >= low then 1 else 0
        (localCount + dfs(len + zero) + dfs(len + one)) % 1_000_000_007
    )

    dfs(len = 0).toInt
