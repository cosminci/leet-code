package com.leetcode.cosminci._300

object _240_SearchIn2DMatrixII:

  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean =
    Iterator
      .iterate((matrix.indices.last, 0)) { case (r, c) =>
        if matrix(r)(c) > target then (r - 1, c)
        else if matrix(r)(c) < target then (r, c + 1)
        else return true
      }
      .dropWhile { case (r, c) => r >= 0 && c < matrix.head.length }
      .next()

    false
