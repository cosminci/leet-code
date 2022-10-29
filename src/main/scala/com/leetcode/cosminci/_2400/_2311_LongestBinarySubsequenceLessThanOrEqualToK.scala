package com.leetcode.cosminci._2400

object _2311_LongestBinarySubsequenceLessThanOrEqualToK:

  def longestSubsequence(s: String, k: Int): Int =
    @annotation.tailrec
    def dfs(i: Int, value: Int, pow: Int, count: Int): Int =
      if i < 0 || value + pow > k then s.count(_ == '0') + count
      else if s(i) == '1' then dfs(i - 1, value + pow, pow << 1, count + 1)
      else dfs(i - 1, value, pow << 1, count)

    dfs(i = s.indices.last, value = 0, pow = 1, count = 0)
