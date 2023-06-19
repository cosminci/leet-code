package com.leetcode.cosminci._2800

object _2718_SumOfMatrixAfterQueries:

  def matrixSumQueries(n: Int, queries: Array[Array[Int]]): Long =
    queries
      .foldRight(0L, Map.empty[Int, Int], Map.empty[Int, Int]) {
        case (Array(t, i, v), (res, rows, cols)) =>
          (t, cols.get(i), rows.get(i)) match
            case (0, None, _) => (res + v.toLong * (n - rows.size), rows, cols.updated(i, v))
            case (1, _, None) => (res + v.toLong * (n - cols.size), rows.updated(i, v), cols)
            case _            => (res, rows, cols)
      }._1
