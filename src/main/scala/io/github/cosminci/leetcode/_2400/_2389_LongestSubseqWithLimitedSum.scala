package io.github.cosminci.leetcode._2400

object _2389_LongestSubseqWithLimitedSum:

  def answerQueries(nums: Array[Int], queries: Array[Int]): Array[Int] =
    val prefixSum = nums.sorted.scanLeft(0)(_ + _)
    queries.map(q => prefixSum.search(q + 1).insertionPoint)
