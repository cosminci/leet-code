package io.github.cosminci.leetcode._300

object _221_MaximalSquare:

  private def maximalSquare(matrix: Array[Array[Char]]): Int =
    val dp  = Array.ofDim[Int](matrix.length, matrix(0).length)
    var max = 0
    matrix.indices.foreach { i =>
      matrix(0).indices.foreach { j =>
        dp(i)(j) = matrix(i)(j) - '0'
        if dp(i)(j) > max then max = dp(i)(j)
      }
    }
    matrix.indices.tail.foreach { i =>
      matrix(0).indices.tail.foreach { j =>
        if matrix(i)(j) == '1' then
          dp(i)(j) = 1 + math.min(dp(i - 1)(j - 1), math.min(dp(i - 1)(j), dp(i)(j - 1)))
          if dp(i)(j) > max then max = dp(i)(j)
      }
    }
    max * max
