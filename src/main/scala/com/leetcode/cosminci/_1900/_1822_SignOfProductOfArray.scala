package com.leetcode.cosminci._1900

object _1822_SignOfProductOfArray:

  def arraySign(nums: Array[Int]): Int =
    val hasZero       = nums.contains(0)
    val negativeCount = nums.count(_ < 0)
    if hasZero then 0 else if negativeCount % 2 == 0 then 1 else -1
