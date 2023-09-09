package com.leetcode.cosminci._2900

object _2829_DetermineMinSumOfKAvoidingArray:

  def minimumSum(n: Int, k: Int): Int =
    @annotation.tailrec
    def dfs(i: Int, set: Set[Int]): Int =
      if set.size == n then set.sum
      else
        val j = Iterator.from(i).dropWhile(j => set.contains(k - j)).next()
        dfs(j + 1, set + j)

    dfs(i = 1, set = Set.empty)
