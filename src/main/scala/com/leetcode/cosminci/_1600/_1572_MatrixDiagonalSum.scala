package com.leetcode.cosminci._1600

object _1572_MatrixDiagonalSum:

  def diagonalSum(mat: Array[Array[Int]]): Int =
    mat.indices
      .flatMap(i => Seq((i, i), (i, mat.length - i - 1)))
      .distinct
      .map { case (i, j) => mat(i)(j) }
      .sum
