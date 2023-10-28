package com.leetcode.cosminci._2900

import com.leetcode.cosminci.utils.decrementCounter

import scala.util.chaining.*

object _2841_MaxSumOfAlmostUniqueSubarray:

  def maxSum(nums: List[Int], m: Int, k: Int): Long =
    val initialCounter = nums.take(k - 1).groupMapReduce(identity)(_ => 1)(_ + _)
    val initialSum     = nums.take(k - 1).map(_.toLong).sum

    nums.indices
      .drop(k - 1)
      .foldLeft(initialCounter, initialSum, 0L) { case ((counter, prevSum, maxSum), i) =>
        val n               = nums(i)
        val expandedCounter = counter.updated(n, counter.getOrElse(n, 0) + 1)
        val currSum         = prevSum + n
        val newMaxSum       = if expandedCounter.size >= m then maxSum.max(currSum) else maxSum
        val shrunkCounter   = decrementCounter(expandedCounter, nums(i - k + 1))
        (shrunkCounter, currSum - nums(i - k + 1), newMaxSum)
      }
      .pipe { case (_, _, maxSum) => maxSum }
