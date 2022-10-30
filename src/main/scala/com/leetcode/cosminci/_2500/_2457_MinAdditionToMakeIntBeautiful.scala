package com.leetcode.cosminci._2500

object _2457_MinAdditionToMakeIntBeautiful:

  def makeIntegerBeautiful(n: Long, target: Int): Long =
    def digitSum(n: Long): Long =
      if n == 0 then 0
      else n % 10 + digitSum(n / 10)

    def dfs(n: Long): Long =
      if digitSum(n) <= target then 0
      else 10 - (n % 10) + dfs(n / 10 + 1)

    dfs(n)
