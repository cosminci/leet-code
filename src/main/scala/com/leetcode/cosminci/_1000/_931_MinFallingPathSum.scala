package com.leetcode.cosminci._1000

object _931_MinFallingPathSum:
  def main(args: Array[String]): Unit =
    println(minFallingPathSum(Array(Array(17, 82), Array(1, -44))))

  def minFallingPathSum(matrix: Array[Array[Int]]): Int =
    (matrix.length - 2 to 0 by -1)
      .foldLeft(matrix.last.toSeq) { case (rowBelow, r) =>
        rowBelow.indices.map { c =>
          matrix(r)(c) + rowBelow.slice(c - 1, c + 2).min
        }
      }.min
