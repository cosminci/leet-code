package com.leetcode.cosminci._100

object _55_JumpGame:

  def canJump(nums: Array[Int]): Boolean =
    nums.indices.foldLeft(0) { (prevMax, i) =>
      if (prevMax < i) return false
      prevMax max (i + nums(i))
    } >= nums.indices.last
