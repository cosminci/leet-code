package com.leetcode.cosminci._2800

object _2732_FindGoodSubsetOfMatrix:

  def goodSubsetofBinaryMatrix(grid: Array[Array[Int]]): List[Int] =
    val (m, n)  = (grid.length, grid.head.length)
    val rowVals = grid.map(row => (0 until n).foldLeft(0)((acc, j) => acc | (row(j) << j)))

    (0 until m).view
      .collectFirst { case i if rowVals(i) == 0 => List(i) }
      .orElse {
        (0 until m).view
          .flatMap(i => (i + 1 until m).map(j => (i, j)))
          .collectFirst { case (i, j) if (rowVals(i) & rowVals(j)) == 0 => List(i, j) }
      }
      .getOrElse(Nil)
