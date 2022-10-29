package com.leetcode.cosminci._700

import com.leetcode.cosminci.utils

import scala.collection.mutable

object _695_MaxAreaOfIsland:

  def maxAreaOfIsland(grid: Array[Array[Int]]): Int =
    def dfs(x: Int, y: Int): Int =
      grid(x)(y) = 0
      1 + utils.neighbours(x, y, grid)
        .collect { case (nx, ny) if grid(nx)(ny) == 1 => dfs(nx, ny) }
        .sum

    val islandSizes = for
      x <- grid.indices
      y <- grid(x).indices if grid(x)(y) == 1
    yield dfs(x, y)

    islandSizes.maxOption.getOrElse(0)
