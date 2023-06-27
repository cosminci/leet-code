package com.leetcode.cosminci._400

import scala.collection.mutable

object _373_FindKPairsWithSmallestSums:

  def kSmallestPairs(nums1: Array[Int], nums2: Array[Int], k: Int): List[List[Int]] =
    val ord: Ordering[(Int, Int)] = { case ((i1, j1), (i2, j2)) =>
      (nums1(i1) + nums2(j1)).compare(nums1(i2) + nums2(j2))
    }
    val next = mutable.PriorityQueue((0, 0))(ord)

    val result = mutable.ListBuffer.empty[List[Int]]
    while next.nonEmpty && result.length < k do
      val (i1, j1) = next.dequeue()
      result.append(List(nums1(i1), nums2(j1)))

      if j1 < nums2.length - 1 then next.enqueue((i1, j1 + 1))
      if j1 == 0 && i1 < nums1.length - 1 then next.enqueue((i1 + 1, j1))

    result.toList
