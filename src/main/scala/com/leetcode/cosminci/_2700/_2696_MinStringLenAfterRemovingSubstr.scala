package com.leetcode.cosminci._2700

object _2696_MinStringLenAfterRemovingSubstr:

  def minLength(s: String): Int =
    s.foldLeft(Seq.empty[Char]) { case (prev, ch) =>
      if ch == 'B' && prev.lastOption.contains('A') then prev.dropRight(1)
      else if ch == 'D' && prev.lastOption.contains('C') then prev.dropRight(1)
      else prev :+ ch
    }.size
