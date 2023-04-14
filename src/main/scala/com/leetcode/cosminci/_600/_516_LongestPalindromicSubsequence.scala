package com.leetcode.cosminci._600

import scala.collection.mutable

object _516_LongestPalindromicSubsequence:

  def longestPalindromeSubseq(s: String): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(l: Int, r: Int): Int = mem.getOrElseUpdate((l, r),
      if l == r then 1
      else if r - l == 1 && s(l) == s(r) then 2
      else if s(l) == s(r) then 2 + dfs(l + 1, r - 1)
      else dfs(l + 1, r).max(dfs(l, r - 1))
    )

    dfs(l = 0, r = s.length - 1)
