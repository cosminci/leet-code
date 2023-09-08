package com.leetcode.cosminci._2900

object _2825_MakeStringASubseqUsingCyclicIncrements:

  def canMakeSubsequence(str1: String, str2: String): Boolean =
    @annotation.tailrec
    def dfs(i: Int, j: Int): Boolean =
      if j == str2.length then true
      else if i == str1.length then false
      else if canMatch(i, j) then dfs(i + 1, j + 1)
      else dfs(i + 1, j)

    def canMatch(i: Int, j: Int): Boolean =
      str1(i) == str2(j) || (str1(i) - 'a' + 1) % 26 == (str2(j) - 'a') % 26

    dfs(i = 0, j = 0)
