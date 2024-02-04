package com.leetcode.cosminci._3100

import scala.util.chaining.*

object _3026_MaxGoodSubarraySum:

  def maximumSubarraySum(nums: Array[Int], k: Int): Long =
    nums.foldLeft(0L, Map.empty[Int, Long], Long.MinValue) { case ((currSum, prevSums, res), n) =>
      val newRes = Seq(n - k, n + k)
        .flatMap(prevSums.get).map(minValue => currSum + n - minValue)
        .maxOption.getOrElse(res).max(res)
      val newPrevSums = prevSums.updated(n, prevSums.getOrElse(n, Long.MaxValue).min(currSum))
      (currSum + n, newPrevSums, newRes)
    }.pipe { case (_, _, res) => if res > Long.MinValue then res else 0 }
