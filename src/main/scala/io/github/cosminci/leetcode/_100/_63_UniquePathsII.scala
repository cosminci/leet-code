package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _63_UniquePathsII:

  def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(x: Int, y: Int): Int = mem.getOrElseUpdate((x, y),
      if obstacleGrid(x)(y) == 1 then 0
      else if x == 0 && y == 0 then 1
      else if x == 0 then dfs(x, y - 1)
      else if y == 0 then dfs(x - 1, y)
      else dfs(x - 1, y) + dfs(x, y - 1)
    )

    dfs(x = obstacleGrid.length - 1, y = obstacleGrid.head.length - 1)
