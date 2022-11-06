package com.leetcode.cosminci._2500

object _2460_ApplyOperationsToArray:

  def applyOperations(nums: Array[Int]): Array[Int] =
    def dfs(i: Int): Array[Int] =
      if i == nums.length then Array.empty
      else if i == nums.indices.last then Array(nums.last)
      else if nums(i) == 0 then dfs(i + 1)
      else if nums(i) != nums(i + 1) then nums(i) +: dfs(i + 1)
      else (nums(i + 1) * 2) +: dfs(i + 2)

    dfs(i = 0).padTo(nums.length, 0)
