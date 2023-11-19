package com.leetcode.cosminci._3000

object _2937_MakeThreeStringsEqual:

  def findMinimumOperations(s1: String, s2: String, s3: String): Int =
    s1.zip(s2).zip(s3).takeWhile { case ((a, b), c) => a == c && b == c }.length match
      case l if l < 1 => -1
      case l =>
        s1.length + s2.length + s3.length - 3 * l
