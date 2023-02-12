package com.leetcode.cosminci._2600

object _2563_CountFairPairs:

  def countFairPairs(nums: Array[Int], lower: Int, upper: Int): Long =
    nums.sortInPlace()

    @annotation.tailrec
    def count(i: Int = 0, j: Int = nums.length - 1, bound: Int, result: Long = 0L): Long =
      if i == j then result
      else if nums(i) + nums(j) > bound then count(i, j - 1, bound, result)
      else count(i + 1, j, bound, result + j - i)

    count(bound = upper) - count(bound = lower - 1)
