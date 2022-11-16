package com.leetcode.cosminci._400

object _374_GuessNumber:

  def guessNumber(n: Int): Int =
    @annotation.tailrec
    def dfs(l: Int, r: Int): Int =
      if l >= r then l
      else
        val mid = l + (r - l) / 2
        if guess(mid) == 0 then mid
        else if guess(mid) < 0 then dfs(l, mid)
        else dfs(mid + 1, r)

    dfs(l = 0, r = n)

  def guess(n: Int): Int = n.compare(5)
