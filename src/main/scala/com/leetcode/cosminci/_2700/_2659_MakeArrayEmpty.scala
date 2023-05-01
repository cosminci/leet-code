package com.leetcode.cosminci._2700

object _2659_MakeArrayEmpty:

  def countOperationsToEmptyArray(nums: Array[Int]): Long =
    val pos = nums.zipWithIndex.toMap
    nums.sortInPlace()
    (1 until nums.length).foldLeft(nums.length.toLong) { case (res, i) =>
      if pos(nums(i)) < pos(nums(i - 1)) then res + nums.length - i else res
    }
