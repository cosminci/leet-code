package com.leetcode.cosminci._400

object _395_LongestSubstringWithAtLeastKRepeatingChars:
  def main(args: Array[String]): Unit =
    println(longestSubstring("aaabb", 3))
    println(longestSubstring("weitong", 2))

  def longestSubstring(s: String, k: Int): Int =
    if s.length < k then 0
    else s.distinct
      .find(char => s.count(_ == char) < k)
      .map(char => s.split(char).map(longestSubstring(_, k)).max)
      .getOrElse(s.length)
