package com.leetcode.cosminci._1000

object _926_FlipStringToMonotoneIncreasing:

  def minFlipsMonoIncr(s: String): Int =
    s.foldLeft(0, 0) { case ((result, ones), ch) =>
      if (ch == '1') (result, ones + 1)
      else (ones.min(result + 1), ones)
    }._1
