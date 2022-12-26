package com.leetcode.cosminci._300

object _283_MoveZeroes:

  def moveZeroes(nums: Array[Int]): Unit =
    nums.indices.foldLeft(0) { case (snowballSize, i) =>
      if nums(i) == 0 then snowballSize + 1
      else if snowballSize == 0 then snowballSize
      else
        val tmp = nums(i)
        nums(i) = 0
        nums(i - snowballSize) = tmp
        snowballSize
    }
