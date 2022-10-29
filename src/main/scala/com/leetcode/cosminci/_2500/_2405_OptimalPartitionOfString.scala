package com.leetcode.cosminci._2500

object _2405_OptimalPartitionOfString:

  def partitionString(s: String): Int =
    s.foldLeft(Set.empty[Char], 1) { case ((seen, cnt), char) =>
      if seen.contains(char) then (Set(char), cnt + 1)
      else (seen + char, cnt)
    }._2
