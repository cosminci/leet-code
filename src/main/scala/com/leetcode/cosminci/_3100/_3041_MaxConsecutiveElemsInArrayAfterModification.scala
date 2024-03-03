package com.leetcode.cosminci._3100

object _3041_MaxConsecutiveElemsInArrayAfterModification:

  def maxSelectedElements(nums: Array[Int]): Int =
    nums.sorted
      .foldLeft(Map.empty[Int, Int].withDefaultValue(0)) { (dp, n) =>
        dp.updated(n + 1, dp(n) + 1).updated(n, dp(n - 1) + 1)
      }.values.max
