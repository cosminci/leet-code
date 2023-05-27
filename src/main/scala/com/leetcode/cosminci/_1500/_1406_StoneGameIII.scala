package com.leetcode.cosminci._1500

import scala.collection.mutable

object _1406_StoneGameIII:

  def stoneGameIII(stoneValue: Array[Int]): String =
    val mem = mutable.Map.empty[Int, Int]
    def dfs(i: Int): Int = mem.getOrElseUpdate(i,
      if i >= stoneValue.length then 0
      else (1 to 3).map { toTake =>
        (i until (i + toTake).min(stoneValue.length)).map(stoneValue).sum - dfs(i + toTake)
      }.max
    )
    if dfs(i = 0) > 0 then "Alice" else if dfs(i = 0) < 0 then "Bob" else "Tie"
