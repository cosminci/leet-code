package com.leetcode.cosminci._100

object _26_RemoveDuplicatesFromSortedArray:

  def removeDuplicates(nums: Array[Int]): Int =
    @annotation.tailrec
    def dfs(i: Int, j: Int): Int =
      if j == nums.length then i + 1
      else if nums(i) == nums(j) then dfs(i, j + 1)
      else
        nums(i + 1) = nums(j)
        dfs(i + 1, j + 1)

    if nums.isEmpty then 0 else dfs(i = 0, j = 0)
