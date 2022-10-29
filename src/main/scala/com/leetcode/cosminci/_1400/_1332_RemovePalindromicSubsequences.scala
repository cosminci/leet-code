package com.leetcode.cosminci._1400

object _1332_RemovePalindromicSubsequences:

  def removePalindromeSub(s: String): Int =
    if s.isEmpty then 0
    else if s == s.reverse then 1
    else 2
