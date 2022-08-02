package io.github.cosminci.leetcode._400

import scala.collection.immutable.TreeSet

object _378_KthSmallestInSortedMatrix:

  def kthSmallest(matrix: Array[Array[Int]], k: Int): Int =
    Iterator
      .iterate(TreeSet.from(matrix.head.zipWithIndex.map { case (v, c) => (v, 0, c) })) { heap =>
        val (_, r, c) = heap.head
        heap.tail ++ Option.when(r < matrix.length - 1)((matrix(r + 1)(c), r + 1, c))
      }
      .drop(k - 1)
      .next()
      .head
      ._1
