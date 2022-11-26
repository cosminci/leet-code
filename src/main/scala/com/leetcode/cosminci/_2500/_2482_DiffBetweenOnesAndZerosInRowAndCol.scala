package com.leetcode.cosminci._2500

import scala.util.chaining.*

object _2482_DiffBetweenOnesAndZerosInRowAndCol:

  def onesMinusZeros(grid: Array[Array[Int]]): Array[Array[Int]] =
    (score(grid), score(grid.transpose)).pipe { case (rowScore, colScore) =>
      Array.tabulate(grid.length, grid.head.length)((x, y) => rowScore(x) + colScore(y))
    }

  private def score(grid: Array[Array[Int]]) =
    grid.map(line => line.count(_ == 1) - line.count(_ == 0))
