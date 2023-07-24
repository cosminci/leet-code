package com.leetcode.cosminci._100

object _50_Pow:

  def myPow(x: Double, n: Int): Double =
    def dfs(num: Double, pow: Int): Double =
      if pow == 0 then 1
      else dfs(num * num, pow / 2) * (if pow % 2 == 0 then 1 else num)

    if n < 0 then 1 / dfs(x, n.abs) else dfs(x, n)
