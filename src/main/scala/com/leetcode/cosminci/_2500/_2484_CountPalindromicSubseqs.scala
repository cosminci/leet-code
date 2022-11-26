package com.leetcode.cosminci._2500

object _2484_CountPalindromicSubseqs:

  def countPalindromes(s: String): Int =
    val mem = Array.fill(s.length, 10, 10, 5)(-1L)
    def dfs(i: Int, first: Int, second: Int, chosen: Int): Long =
      if chosen == 5 then 1
      else if i == s.length then 0
      else if mem(i)(first)(second)(chosen) != -1L then mem(i)(first)(second)(chosen)
      else
        val skip      = dfs(i + 1, first, second, chosen)
        val choose1st = if chosen != 0 then 0 else dfs(i + 1, s(i) - '0', second, chosen = 1)
        val choose2nd = if chosen != 1 then 0 else dfs(i + 1, first, s(i) - '0', chosen = 2)
        val choose3rd = if chosen != 2 then 0 else dfs(i + 1, first, second, chosen = 3)
        val choose4th = if chosen != 3 || s(i) - '0' != second then 0 else dfs(i + 1, first, second, chosen = 4)
        val choose5th = if chosen != 4 || s(i) - '0' != first then 0 else dfs(i + 1, first, second, chosen = 5)

        val result = (skip + choose1st + choose2nd + choose3rd + choose4th + choose5th) % 1_000_000_007
        mem(i)(first)(second)(chosen) = result
        result

    dfs(i = 0, first = 0, second = 0, chosen = 0).toInt
