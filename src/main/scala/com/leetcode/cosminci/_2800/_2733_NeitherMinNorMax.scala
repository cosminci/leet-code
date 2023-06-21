package com.leetcode.cosminci._2800

object _2733_NeitherMinNorMax:

  def findNonMinOrMax(nums: Array[Int]): Int =
    val (min, max) = (nums.min, nums.max)
    nums.find(n => n != min && n != max).getOrElse(-1)
