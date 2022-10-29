package com.leetcode.cosminci._2200

object _2178_MaxSplitOfPositiveEvenInts:

  def maximumEvenSplit(finalSum: Long): List[Long] =
    @annotation.tailrec
    def dfs(nums: List[Long], sum: Long, next: Long): List[Long] =
      if sum + next > finalSum then (finalSum - sum + nums.head) +: nums.tail
      else dfs(next +: nums, sum + next, next + 2)

    if finalSum % 2 == 1 then List.empty
    else dfs(List.empty, sum = 0, next = 2)
