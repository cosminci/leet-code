package com.leetcode.cosminci._3000

object _2909_MinSumOfMountainTripletsII:

  def minimumSum(nums: Array[Int]): Int =
    val pMin = nums.scanLeft(Int.MaxValue)(_ min _)
    val sMin = nums.scanRight(Int.MaxValue)(_ min _).tail

    nums.indices
      .collect { case i if pMin(i) < nums(i) && nums(i) > sMin(i) => pMin(i) + nums(i) + sMin(i) }
      .minOption.getOrElse(-1)
