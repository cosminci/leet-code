package com.leetcode.cosminci._3100

object _3084_CountSubstrStartingAndEndingWithChar:

  def countSubstrings(s: String, c: Char): Long =
    val n = s.count(_ == c)
    n.toLong * (n + 1) / 2
