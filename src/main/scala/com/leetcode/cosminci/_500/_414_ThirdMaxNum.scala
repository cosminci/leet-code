package com.leetcode.cosminci._500

object _414_ThirdMaxNum:

  def thirdMax(nums: Array[Int]): Int =
    if nums.distinct.length < 3 then nums.max
    else nums.distinct.sorted.takeRight(3).head
