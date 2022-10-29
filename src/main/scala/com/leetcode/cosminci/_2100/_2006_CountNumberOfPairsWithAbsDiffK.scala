package com.leetcode.cosminci._2100

object _2006_CountNumberOfPairsWithAbsDiffK:
  def countKDifferenceBruteForce(nums: Array[Int], k: Int): Int =
    nums.indices.combinations(2).count { case Seq(i, j) => math.abs(nums(j) - nums(i)) == k }

  def countKDifferenceFreqMap(nums: Array[Int], k: Int): Int = {
    val counter = nums.groupBy(identity).mapValues(_.length)
    counter.foldLeft(0) {
      case (count, (n, freq)) =>
        count + counter.getOrElse(n + k, 0) * freq
    }
  }
