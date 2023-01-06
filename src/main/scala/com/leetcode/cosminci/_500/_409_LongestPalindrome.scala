package com.leetcode.cosminci._500

object _409_LongestPalindrome:

  def longestPalindrome(s: String): Int =
    val counts = s.groupMapReduce(identity)(_ => 1)(_ + _).values.toSeq
    counts.map { c => if c % 2 == 1 then c - 1 else c }.sum + Option.when(counts.exists(_ % 2 == 1))(1).getOrElse(0)
