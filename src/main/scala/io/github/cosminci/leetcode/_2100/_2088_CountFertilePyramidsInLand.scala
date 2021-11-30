package io.github.cosminci.leetcode._2100

import scala.collection.mutable

object _2088_CountFertilePyramidsInLand:
  def countPyramids(grid: Array[Array[Int]]): Int =
    val (m, n) = (grid.length, grid.head.length)

    val mem = mutable.Map.empty[(Int, Int, Int), Int]
    def dfs(x: Int, y: Int, dr: Int): Int = mem.getOrElseUpdate((x, y, dr), {
      if grid(x)(y) == 1 && 0 <= x + dr && x + dr < m && y > 0 && y < n - 1 && grid(x + dr)(y) == 1 then
        1 + dfs(x + dr, y - 1, dr).min(dfs(x + dr, y + 1, dr))
      else grid(x)(y)
    })

    val counts = for
      x <- 0 until m
      y <- 0 until n
    yield 0.max(dfs(x, y, dr = 1) - 1) + 0.max(dfs(x, y, dr = -1) - 1)

    counts.sum
