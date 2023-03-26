package com.leetcode.cosminci._2700

object _2602_MinOpsToMakeArrayElementsEqual:

  def minOperations(nums: Array[Int], queries: Array[Int]): List[Long] =
    nums.sortInPlace()
    val pSum = nums.scanLeft(0L)(_ + _)
    queries.map { q =>
      val splitIdx           = nums.search(q).insertionPoint
      val addToPrefix        = q.toLong * splitIdx - pSum(splitIdx)
      val subtractFromSuffix = pSum.last - pSum(splitIdx) - q.toLong * (nums.length - splitIdx)
      addToPrefix + subtractFromSuffix
    }.toList
