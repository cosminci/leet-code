package com.leetcode.cosminci._1000

object _991_BrokenCalculator:
  def brokenCalc(startValue: Int, target: Int): Int =
    @annotation.tailrec
    def dfs(x: Int, y: Int, cnt: Int): Int =
      if x > y then x - y + cnt
      else if x == y then cnt
      else if y % 2 == 0 then dfs(x, y / 2, cnt + 1)
      else dfs(x, y + 1, cnt + 1)

    dfs(x = startValue, y = target, cnt = 0)
