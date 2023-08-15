package com.leetcode.cosminci._2900

object _2815_MaxPairSumInArray:

  def maxSum(nums: Array[Int]): Int =
    nums
      .groupBy(_.toString.max)
      .values
      .flatMap(nums => Option.when(nums.length >= 2)(nums.sorted.takeRight(2).sum))
      .maxOption
      .getOrElse(-1)
