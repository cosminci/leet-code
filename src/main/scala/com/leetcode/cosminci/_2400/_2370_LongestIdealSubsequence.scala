package com.leetcode.cosminci._2400

object _2370_LongestIdealSubsequence:

  def longestIdealString(s: String, k: Int): Int =
    s.foldLeft(Map.empty[Char, Int]) { (longestByLastChar, char) =>
      longestByLastChar.updated(char,
        (-k to k)
          .map(shift => (char + shift).toChar)
          .flatMap(longestByLastChar.get)
          .maxOption
          .getOrElse(0) + 1
      )
    }.values.max
