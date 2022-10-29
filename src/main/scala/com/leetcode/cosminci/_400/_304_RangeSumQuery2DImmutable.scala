package com.leetcode.cosminci._400

object _304_RangeSumQuery2DImmutable:
  def main(args: Array[String]): Unit =
    val matrix = new NumMatrix(
      Array(
        Array(3, 0, 1, 4, 2),
        Array(5, 6, 3, 2, 1),
        Array(1, 2, 0, 1, 5),
        Array(4, 1, 0, 1, 7),
        Array(1, 0, 3, 0, 5)
      )
    )
    println(matrix.sumRegion(2, 1, 4, 3))
    println(matrix.sumRegion(1, 1, 2, 2))
    println(matrix.sumRegion(1, 2, 2, 4))

  class NumMatrix(matrix: Array[Array[Int]]):
    private val (m, n) = (matrix.length, matrix.head.length)
    private val sums   = Array.ofDim[Int](m + 1, n + 1)

    matrix.indices.foreach { i =>
      matrix(i).indices.foreach { j =>
        sums(i + 1)(j + 1) = matrix(i)(j) + sums(i)(j + 1) + sums(i + 1)(j) - sums(i)(j)
      }
    }

    def sumRegion(row1: Int, col1: Int, row2: Int, col2: Int): Int =
      sums(row2 + 1)(col2 + 1) - sums(row1)(col2 + 1) - sums(row2 + 1)(col1) + sums(row1)(col1)
