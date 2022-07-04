package io.github.cosminci.leetcode._2400

import io.github.cosminci.utils

import scala.collection.mutable

object _2328_NumIncreasingPathsInGrid:

  def countPaths(grid: Array[Array[Int]]): Int =
    val (m, n) = (grid.length, grid.head.length)
    val mod    = 1_000_000_007
    val mem    = mutable.Map.empty[(Int, Int), Long]

    def dfs(x: Int, y: Int): Long = mem.getOrElseUpdate((x, y),
      1 + utils.neighbours(x, y, grid).collect {
        case (nx, ny) if grid(nx)(ny) < grid(x)(y) => dfs(nx, ny)
      }.sum % mod
    )

    val counts = for
      x <- 0 until m
      y <- 0 until n
    yield dfs(x, y)

    counts.foldLeft(0L)((total, count) => (total + count) % mod).toInt
