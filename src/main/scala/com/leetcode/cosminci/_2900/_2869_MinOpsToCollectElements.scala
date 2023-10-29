package com.leetcode.cosminci._2900

object _2869_MinOpsToCollectElements:

  def minOperations(nums: List[Int], k: Int): Int =
    @annotation.tailrec
    def dfs(i: Int, need: Set[Int]): Int =
      if (need - nums(i)).isEmpty then nums.length - i
      else dfs(i - 1, need - nums(i))

    dfs(i = nums.indices.last, need = Set.from(1 to k))
