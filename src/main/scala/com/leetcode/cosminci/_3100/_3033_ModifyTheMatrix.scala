package com.leetcode.cosminci._3100

object _3033_ModifyTheMatrix:

  def modifiedMatrix(matrix: Array[Array[Int]]): Array[Array[Int]] =
    val maxPerCol = matrix.transpose.map(_.max)
    Array.tabulate(matrix.length, matrix(0).length) { (i, j) =>
      if matrix(i)(j) != -1 then matrix(i)(j)
      else maxPerCol(j)
    }
