package io.github.cosminci.leetcode._100

object _63_UniquePathsII:
  def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int =
    val m  = obstacleGrid.length
    val n  = obstacleGrid.head.length
    val dp = Array.ofDim[Int](m + 1, n + 1)
    dp(0)(1) = 1
    (1 to m).foreach { i =>
      (1 to n).foreach { j =>
        dp(i)(j) =
          if obstacleGrid(i - 1)(j - 1) == 1 then 0
          else dp(i - 1)(j) + dp(i)(j - 1)
      }
    }
    dp(m)(n)
