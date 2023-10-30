package com.leetcode.cosminci._3000

object _2905_FindIndicesWithIndexAndValueDiffII:

  def findIndices(nums: Array[Int], indexDifference: Int, valueDifference: Int): Array[Int] =
    (indexDifference until nums.length).foldLeft(0, 0) { case ((min, max), i) =>
      val newMin = if nums(i - indexDifference) < nums(min) then i - indexDifference else min
      val newMax = if nums(i - indexDifference) > nums(max) then i - indexDifference else max
      if nums(i) - nums(newMin) >= valueDifference then return Array(newMin, i)
      if nums(newMax) - nums(i) >= valueDifference then return Array(newMax, i)
      (newMin, newMax)
    }
    Array(-1, -1)
