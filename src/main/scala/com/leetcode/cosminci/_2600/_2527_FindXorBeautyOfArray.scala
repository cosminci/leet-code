package com.leetcode.cosminci._2600

object _2527_FindXorBeautyOfArray:

  def xorBeauty(nums: Array[Int]): Int =
    nums.reduce(_ ^ _)
