package io.github.cosminci.leetcode._1400

import scala.collection.mutable

object _1329_SortMatrixDiagonally:

  def diagonalSort(mat: Array[Array[Int]]): Array[Array[Int]] =
    val sortedDiags = mat.indices
      .flatMap(r => mat.head.indices.map(c => (r, c)))
      .groupBy { case (r, c) => r - c }
      .view
      .mapValues(values => mutable.PriorityQueue.from(values.map { case (r, c) => -mat(r)(c) }))
      .toMap

    Array.tabulate(mat.length, mat.head.length) { case (r, c) =>
      -sortedDiags(r - c).dequeue()
    }
