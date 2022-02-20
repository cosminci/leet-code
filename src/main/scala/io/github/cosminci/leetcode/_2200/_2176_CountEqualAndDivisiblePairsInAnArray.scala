package io.github.cosminci.leetcode._2200

object _2176_CountEqualAndDivisiblePairsInAnArray:

  def countPairs(nums: Array[Int], k: Int): Int =
    nums.indices
      .combinations(2)
      .count { case Seq(i, j) =>
        nums(i) == nums(j) && i * j % k == 0
      }
