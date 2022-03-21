package io.github.cosminci.leetcode._2300

object _2207_MaximizeNumOfSubseqsInString:
  def maximumSubsequenceCount(text: String, pattern: String): Long =
    val (sum, cnt1, cnt2) = text.foldLeft(0L, 0L, 0L) {
      case ((sum, cnt1, cnt2), c) => (
        if c == pattern.last then sum + cnt1 else sum,
        if c == pattern.head then cnt1 + 1 else cnt1,
        if c == pattern.last then cnt2 + 1 else cnt2
      )
    }
    sum + cnt1.max(cnt2)
