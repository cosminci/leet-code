package com.leetcode.cosminci._2900

object _2834_FindMinPossibleSumOfBeautifulArray:

  def minimumPossibleSum(n: Int, target: Int): Int =
    @annotation.tailrec
    def dfs(i: Int, set: Set[Long]): Int =
      if set.size == n then set.foldLeft(0L) { case (a, b) => (a + b) % 1_000_000_007 }.toInt
      else
        val j = Iterator.from(i).dropWhile(j => set.contains(target - j)).next()
        dfs(j + 1, set + j)

    dfs(i = 1, set = Set.empty)
