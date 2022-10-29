package com.leetcode.cosminci._2500

object _2441_LargestPosIntThatExistsWithItsNegative:

  def findMaxK(nums: Array[Int]): Int =
    val distinct = nums.toSet
    nums.foldLeft(-1) { (res, n) =>
      if nums.contains(-n) then res.max(n) else res
    }
