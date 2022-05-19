package io.github.cosminci.leetcode._400

import io.github.cosminci.utils

import scala.collection.mutable

object _329_LongestIncreasingPathInAMatrix:

  def longestIncreasingPath(matrix: Array[Array[Int]]): Int =
    val (m, n)                 = (matrix.length, matrix.head.length)
    def inGrid(x: Int, y: Int) = x >= 0 && x < m && y >= 0 && y < n

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(x: Int, y: Int): Int = mem.getOrElseUpdate((x, y),
      Seq((-1, 0), (0, -1), (0, 1), (1, 0))
        .collect { case (dx, dy) if inGrid(x + dx, y + dy) => (x + dx, y + dy) }
        .collect { case (nx, ny) if matrix(nx)(ny) < matrix(x)(y) => dfs(nx, ny) }
        .maxOption
        .getOrElse(0) + 1
    )

    (0 until m).flatMap(x => (0 until n).map(y => dfs(x, y))).max
