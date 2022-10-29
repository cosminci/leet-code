package com.leetcode.cosminci._2400

import scala.collection.mutable

object _2400_NumWaysToReachPosAfterKSteps:

  def numberOfWays(startPos: Int, endPos: Int, k: Int): Int =
    val mem = mutable.Map.empty[(Int, Int), Long]
    def dfs(pos: Int, k: Int): Long =
      mem.getOrElseUpdate((pos, k),
        if k == 0 && pos == endPos then 1
        else if k == 0 then 0
        else (dfs(pos + 1, k - 1) + dfs(pos - 1, k - 1)) % 1_000_000_007
      )

    dfs(startPos, k).toInt
