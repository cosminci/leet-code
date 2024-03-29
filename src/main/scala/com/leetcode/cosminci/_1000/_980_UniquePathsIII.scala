package com.leetcode.cosminci._1000

import com.leetcode.cosminci.utils

object _980_UniquePathsIII:
  def main(args: Array[String]): Unit =
    println(
      uniquePathsIII(
        Array(
          Array(1, 0, 0, 0),
          Array(0, 0, 0, 0),
          Array(0, 0, 2, -1)
        )
      )
    )

  def uniquePathsIII(grid: Array[Array[Int]]): Int =
    val cells          = grid.indices.flatMap(r => grid(r).indices.map(c => (r, c)))
    val Some((x0, y0)) = cells.find { case (x, y) => grid(x)(y) == 1 }
    val emptyCellCount = cells.count { case (x, y) => grid(x)(y) == 0 }

    def dfs(x: Int, y: Int, emptyLeft: Int): Int =
      if grid(x)(y) < 0 then return 0
      if grid(x)(y) == 2 then return if emptyLeft == 0 then 1 else 0

      grid(x)(y) = -2
      val result = utils.neighbours(x, y, grid).map {
        case (nx, ny) =>
          dfs(nx, ny, emptyLeft - 1)
      }.sum
      grid(x)(y) = 0
      result

    dfs(x0, y0, emptyCellCount + 1)
