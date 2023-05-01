package com.leetcode.cosminci._2700

object _2656_MaxSumWithExactlyKElements:

  def maximizeSum(nums: Array[Int], k: Int): Int =
    nums.max * k + k * (k - 1) / 2
