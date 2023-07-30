package com.leetcode.cosminci._700

import scala.collection.mutable

object _664_StrangePrinter:

  def strangePrinter(s: String): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(l: Int, r: Int): Int = mem.getOrElseUpdate((l, r),
      if l > r then 0
      else (l until r - 1).foldLeft(dfs(l, r - 1) + 1) { (minTurns, mid) =>
        if s(mid) != s(r - 1) then minTurns
        else minTurns.min(dfs(l, mid) + dfs(mid + 1, r))
      }
    )
    dfs(l = 0, r = s.length)
