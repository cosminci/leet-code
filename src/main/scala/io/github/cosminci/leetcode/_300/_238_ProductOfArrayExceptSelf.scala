package io.github.cosminci.leetcode._300

object _238_ProductOfArrayExceptSelf:
  def productExceptSelf(nums: Array[Int]): Array[Int] =
    val dp = Array.ofDim[Int](nums.length)
    dp(0) = 1
    (1 until dp.length).foreach { i =>
      dp(i) = dp(i - 1) * nums(i - 1)
    }
    var suffix = 1
    (dp.length - 1 to 0 by -1).foreach { i =>
      dp(i) = dp(i) * suffix
      suffix *= nums(i)
    }
    dp
