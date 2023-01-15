package com.leetcode.cosminci._2600

object _2535_DiffBetweenElementSumAndDigitSum:

  def differenceOfSum(nums: Array[Int]): Int =
    nums.sum - nums.map(_.toString.map(_ - '0').sum).sum
