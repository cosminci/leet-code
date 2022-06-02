package io.github.cosminci.leetcode._900

object _867_TransposeMatrix:

  def transpose(matrix: Array[Array[Int]]): Array[Array[Int]] =
    val (m, n) = (matrix.length, matrix.head.length)
    Array.tabulate(n)(r => Array.tabulate(m)(c => matrix(c)(r)))
