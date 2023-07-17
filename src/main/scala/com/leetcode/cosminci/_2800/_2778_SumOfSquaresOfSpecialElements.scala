package com.leetcode.cosminci._2800

object _2778_SumOfSquaresOfSpecialElements:

  def sumOfSquares(nums: Array[Int]): Int =
    nums.view
      .indices
      .filter(i => nums.length % (i + 1) == 0)
      .map(i => nums(i) * nums(i))
      .sum
