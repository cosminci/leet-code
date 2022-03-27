package io.github.cosminci.leetcode._1400

object _1337_KWeakestRowsInMatrix:

  def kWeakestRows(mat: Array[Array[Int]], k: Int): Array[Int] =
    mat.zipWithIndex
      .sortBy { case (row, idx) => (row.takeWhile(_ == 1).length, idx) }
      .map { case (_, idx) => idx }
      .take(k)
