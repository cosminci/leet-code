package io.github.cosminci.leetcode._1500

object _1461_CheckIfStringContainsAllBinaryCodesOfSizeK:

  def hasAllCodes(s: String, k: Int): Boolean =
    s.sliding(k).distinct.length == 1 << k
