package com.leetcode.cosminci._2800

import scala.collection.mutable

object _2742_PaintingTheWalls:

  def paintWalls(cost: Array[Int], time: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, t: Int): Int = mem.getOrElseUpdate((i, t),
      if i == cost.length then if t >= 0 then 0 else 1e9.toInt
      else (cost(i) + dfs(i + 1, (t + time(i)).min(time.length))).min(dfs(i + 1, t - 1))
    )
    dfs(i = 0, t = 0)
