package com.leetcode.cosminci._1100

import scala.collection.mutable

object _1044_LongestDuplicateSubstring:
  def main(args: Array[String]): Unit =
    println(longestDupSubstring("banana"))

  def longestDupSubstring(s: String): String =
    var maxLength = 1
    var longest   = ""

    (0 until s.length).foreach { start =>
      var suffix    = s.slice(start + 1, s.length)
      var candidate = s.slice(start, start + maxLength)

      while suffix.contains(candidate) do
        longest = candidate
        maxLength += 1
        candidate = s.slice(start, start + maxLength)
    }

    longest
