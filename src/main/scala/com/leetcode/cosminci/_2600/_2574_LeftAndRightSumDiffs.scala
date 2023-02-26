package com.leetcode.cosminci._2600

object _2574_LeftAndRightSumDiffs:

  def leftRigthDifference(nums: Array[Int]): Array[Int] =
    nums.scanLeft(0)(_ + _)
      .zip(nums.scanRight(0)(_ + _).tail)
      .map { case (l, r) => (l - r).abs }
