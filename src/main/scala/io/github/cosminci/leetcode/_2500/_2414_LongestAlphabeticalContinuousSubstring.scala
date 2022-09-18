package io.github.cosminci.leetcode._2500

object _2414_LongestAlphabeticalContinuousSubstring:

  def longestContinuousSubstring(s: String): Int =
    s.indices.tail
      .foldLeft(1, 1) { case ((curr, max), i) =>
        if s(i) != s(i - 1) + 1 then (1, max)
        else (curr + 1, max.max(curr + 1))
      }._2
