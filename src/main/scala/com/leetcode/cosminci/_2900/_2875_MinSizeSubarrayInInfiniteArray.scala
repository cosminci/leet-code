package com.leetcode.cosminci._2900

import scala.util.chaining.*

object _2875_MinSizeSubarrayInInfiniteArray:

  def minSizeSubarray(nums: Array[Int], target: Int): Int =
    val sum = nums.sum
    val tgt = target - target / sum * sum
    Iterator
      .iterate((0, 0, 0, Int.MaxValue)) { case (i, j, sum, dist) =>
        Iterator
          .iterate((sum + nums(i % nums.length), j)) { case (sum, j) => (sum - nums(j % nums.length), j + 1) }
          .dropWhile { case (sum, _) => sum > tgt }.next()
          .pipe { case (sum, j) => (i + 1, j, sum, if sum != tgt then dist else dist.min(i - j + 1)) }
      }
      .dropWhile { case (i, _, _, _) => i < 2 * nums.length }.next()
      .pipe { case (_, _, _, dist) => if dist == Int.MaxValue then -1 else target / sum * nums.length + dist }
