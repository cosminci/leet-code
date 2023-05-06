package com.leetcode.cosminci._1500

import scala.util.chaining.*

object _1498_NumSubseqsThatSatisfyGivenSumCondition:

  def numSubseq(nums: Array[Int], target: Int): Int =
    val mod  = 1_000_000_007
    val pows = (1 until nums.length).scanLeft(1)((prev, i) => prev * 2 % mod)

    nums.sortInPlace()

    Iterator
      .iterate((0, nums.length - 1, 0L)) { case (l, r, res) =>
        if nums(l) + nums(r) > target then (l, r - 1, res)
        else (l + 1, r, (res + pows(r - l)) % mod)
      }
      .dropWhile { case (l, r, _) => l <= r }.next()
      .pipe { case (_, _, res) => res.toInt }
