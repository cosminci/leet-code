package com.leetcode.cosminci._2600

object _2546_ApplyBitwiseOpsToMakeStringsEq:

  def makeStringsEqual(s: String, target: String): Boolean =
    s.exists(_ == '1') == target.exists(_ == '1')
