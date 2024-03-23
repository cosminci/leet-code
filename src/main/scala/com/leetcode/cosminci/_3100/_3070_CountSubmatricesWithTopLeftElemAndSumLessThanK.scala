package com.leetcode.cosminci._3100

object _3070_CountSubmatricesWithTopLeftElemAndSumLessThanK:

  def countSubmatrices(grid: Array[Array[Int]], k: Int): Int =
    val accumulatedRows = grid.map(_.scanLeft(0)(_ + _).tail)
    val accumulatedGrid = accumulatedRows.transpose.map(_.scanLeft(0)(_ + _).tail)
    accumulatedGrid.flatten.count(_ <= k)
