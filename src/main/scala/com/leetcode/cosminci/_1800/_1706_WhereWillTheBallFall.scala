package com.leetcode.cosminci._1800

import scala.collection.mutable

object _1706_WhereWillTheBallFall:

  def findBall(grid: Array[Array[Int]]): Array[Int] =
    val (m, n) = (grid.length, grid.head.length)

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(r: Int, c: Int): Int = mem.getOrElseUpdate((r, c),
      if r == m then c
      else if grid(r)(c) == 1 then
        if c == n - 1 || grid(r)(c + 1) == -1 then -1
        else dfs(r + 1, c + 1)
      else
        if c == 0 || grid(r)(c - 1) == 1 then -1
        else dfs(r + 1, c - 1)
    )

    (0 until n).map(c => dfs(r = 0, c)).toArray
