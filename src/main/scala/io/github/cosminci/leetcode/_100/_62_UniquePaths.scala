package io.github.cosminci.leetcode._100

object _62_UniquePaths:
  private def uniquePaths(m: Int, n: Int): Int =
    val dp = Array.ofDim[Int](m + 1, n + 1)
    dp(0)(1) = 1
    (1 to m).foreach { i =>
      (1 to n).foreach { j =>
        dp(i)(j) = dp(i - 1)(j) + dp(i)(j - 1)
      }
    }
    dp(m)(n)
