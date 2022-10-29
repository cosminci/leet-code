package com.leetcode.cosminci._900

object _861_ScoreAfterFlippingMatrix:

  def matrixScore(grid: Array[Array[Int]]): Int =
    val (m, n) = (grid.length, grid(0).length)

    (1 until n).foldLeft(m * 1 << n - 1) { (score, col) =>
      val colScore = (0 until m).count(row => grid(row)(col) == grid(row)(0))
      score + colScore.max(m - colScore) * (1 << n - 1 - col)
    }
