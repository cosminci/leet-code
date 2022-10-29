package com.leetcode.cosminci._1000

object _905_SortArrayByParity:

  def sortArrayByParity(nums: Array[Int]): Array[Int] =
    val (even, odd) = nums.partition(_ % 2 == 0)
    even ++ odd
