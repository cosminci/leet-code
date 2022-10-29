package com.leetcode.cosminci._100

object _1_TwoSum:

  def twoSum(nums: Array[Int], target: Int): Array[Int] =
    nums.indices.foldLeft(Map.empty[Int, Int]) { (seen, idx) =>
      if seen.contains(target - nums(idx)) then
        return Array(seen(target - nums(idx)), idx)
      seen.updated(nums(idx), idx)
    }
    Array.empty
