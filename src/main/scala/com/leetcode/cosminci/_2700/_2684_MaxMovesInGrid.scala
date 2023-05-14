package com.leetcode.cosminci._2700

import scala.util.chaining.*

object _2684_MaxMovesInGrid:

  def maxMoves(grid: Array[Array[Int]]): Int =
    val (m, n) = (grid.length, grid.head.length)

    def dfs(toVisit: Set[(Int, Int)]): Int =
      if toVisit.isEmpty then -1
      else
        toVisit.flatMap { case (r, c) =>
          Seq((r - 1, c + 1), (r, c + 1), (r + 1, c + 1))
            .filter { case (nr, nc) => nc < n && nr >= 0 && nr < m && grid(nr)(nc) > grid(r)(c) }
        }.pipe(toVisit => 1 + dfs(toVisit))

    dfs(grid.indices.map(r => (r, 0)).toSet)
