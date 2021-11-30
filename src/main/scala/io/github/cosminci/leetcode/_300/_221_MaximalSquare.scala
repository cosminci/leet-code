package io.github.cosminci.leetcode._300

object _221_MaximalSquare:

  def maximalSquare(matrix: Array[Array[Char]]): Int =
    val dp  = Array.ofDim[Int](matrix.length, matrix(0).length)
    var max = 0
    matrix.indices.foreach { i =>
      matrix(0).indices.foreach { j =>
        dp(i)(j) = matrix(i)(j) - '0'
        max = dp(i)(j).max(max)
      }
    }
    matrix.indices.tail.foreach { i =>
      matrix(0).indices.tail.foreach { j =>
        if matrix(i)(j) == '1' then
          dp(i)(j) = 1 + dp(i - 1)(j - 1).min(dp(i - 1)(j)).min(dp(i)(j - 1))
          max = dp(i)(j).max(max)
      }
    }
    max * max
