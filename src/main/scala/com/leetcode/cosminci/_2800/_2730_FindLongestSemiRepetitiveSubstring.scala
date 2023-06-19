package com.leetcode.cosminci._2800

object _2730_FindLongestSemiRepetitiveSubstring:

  def longestSemiRepetitiveSubstring(s: String): Int =
    s.length - (1 until s.length)
      .foldLeft(0, 0) { case ((i, curr), j) =>
        val toAdd = if s(j - 1) == s(j) then 1 else 0
        if curr + toAdd <= 1 then (i, curr + toAdd)
        else
          val toRemove = if s(i) == s(i + 1) then 1 else 0
          (i + 1, curr + toAdd - toRemove)
      }._1
