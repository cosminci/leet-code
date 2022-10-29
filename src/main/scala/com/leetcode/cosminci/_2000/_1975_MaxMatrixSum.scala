package com.leetcode.cosminci._2000

object _1975_MaxMatrixSum:
  def main(args: Array[String]): Unit =
    println(maxMatrixSum(Array(Array(1, 2, 3), Array(-1, -2, -3), Array(1, 2, 3))))

  def maxMatrixSum(matrix: Array[Array[Int]]): Long =
    var (total, min, negatives) = (0L, Int.MaxValue, 0)
    matrix.foreach { row =>
      row.foreach { value =>
        total += math.abs(value)
        min = math.min(min, math.abs(value))
        if value < 0 then negatives += 1
      }
    }
    if negatives % 2 == 1 then total - 2 * min else total
