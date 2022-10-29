package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils

object _200_NumberOfIslands:

  def numIslands(grid: Array[Array[Char]]): Int =
    def sink(x: Int, y: Int): Unit =
      grid(x)(y) = 0
      utils
        .neighbours(x, y, grid)
        .filter { case (x, y) => grid(x)(y) == '1' }
        .foreach(sink)

    val sinkOps = for
      x <- grid.indices
      y <- grid.head.indices
      if grid(x)(y) == '1'
    yield sink(x, y)

    sinkOps.length
