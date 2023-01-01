package com.leetcode.cosminci._2600

object _2520_CountDigitsThatDivideNum:

  def countDigits(num: Int): Int =
    @annotation.tailrec
    def dfs(n: Int, res: Int): Int =
      if (n == 0) res
      else dfs(n / 10, if (num % (n % 10) == 0) res + 1 else res)

    dfs(num, res = 0)
