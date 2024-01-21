package com.leetcode.cosminci._3100

import com.leetcode.cosminci.utils

object _3012_MinimizeLenOfArrayUsingOps:

  def minimumArrayLength(nums: Array[Int]): Int =
    val gcd = nums.reduce(utils.gcd)
    (1.max(nums.count(_ == gcd)) + 1) / 2
