package com.leetcode.cosminci._2200

object _2180_CountIntsWithEvenDigitSum:

  def countEven(num: Int): Int =
    def dfs(n: Int): Int =
      if n == 0 then 0
      else n % 10 + dfs(n / 10)

    (num - dfs(num) % 2) / 2
