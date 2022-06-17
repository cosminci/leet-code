package io.github.cosminci.leetcode._2400

import scala.collection.mutable

object _2304_MinPathCostInGrid:

  def minPathCost(grid: Array[Array[Int]], moveCost: Array[Array[Int]]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(x: Int, y: Int): Int = mem.getOrElseUpdate((x, y),
      if x == grid.length - 1 then grid(x)(y)
      else grid(x)(y) + grid.head.indices
        .filter(ny => ny >= 0 && ny < grid.head.length)
        .map(ny => moveCost(grid(x)(y))(ny) + dfs(x + 1, ny))
        .min
    )

    grid.head.indices.map(y => dfs(x = 0, y)).min
