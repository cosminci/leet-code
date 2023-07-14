package com.leetcode.cosminci._1300

object _1218_LongestArithmeticSubseqOfGivenDiff:

  def longestSubsequence(arr: Array[Int], diff: Int): Int =
    arr.foldLeft(Map.empty[Int, Int])((res, n) => res.updated(n, res.getOrElse(n - diff, 0) + 1)).values.max
