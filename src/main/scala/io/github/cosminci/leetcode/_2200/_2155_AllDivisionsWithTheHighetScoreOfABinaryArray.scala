package io.github.cosminci.leetcode._2200

object _2155_AllDivisionsWithTheHighetScoreOfABinaryArray:
  def maxScoreIndices(nums: Array[Int]): List[Int] = {
    val zeroesToTheLeft = nums.scanLeft(0) { (count, n) => count + Option.when(n == 0)(1).getOrElse(0) }
    val onesToTheRight  = nums.scanRight(0) { (n, count) => count + Option.when(n == 1)(1).getOrElse(0) }

    (0 to nums.length)
      .groupBy(i => zeroesToTheLeft(i) + onesToTheRight(i))
      .maxBy { case (count, indices) => count }
      ._2
      .toList
  }
