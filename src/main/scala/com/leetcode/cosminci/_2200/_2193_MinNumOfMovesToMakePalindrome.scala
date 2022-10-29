package com.leetcode.cosminci._2200

object _2193_MinNumOfMovesToMakePalindrome:

  def minMovesToMakePalindrome(s: String): Int =
    @annotation.tailrec
    def dfs(s: String, res: Int): Int =
      if s.isEmpty then res
      else
        val idx = s.indexOf(s.last)
        if idx == s.length - 1 then dfs(s.dropRight(1), res + idx / 2)
        else dfs(s.patch(idx, Nil, 1).dropRight(1), res + idx)
    dfs(s, res = 0)
