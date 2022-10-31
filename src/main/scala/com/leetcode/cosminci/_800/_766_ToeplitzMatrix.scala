package com.leetcode.cosminci._800

object _766_ToeplitzMatrix:

  def isToeplitzMatrix(matrix: Array[Array[Int]]): Boolean =
    matrix.indices.tail.forall { r =>
      matrix(r).indices.tail.forall { c =>
        matrix(r)(c) == matrix(r - 1)(c - 1)
      }
    }
