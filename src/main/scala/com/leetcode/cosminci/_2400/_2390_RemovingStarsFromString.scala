package com.leetcode.cosminci._2400

object _2390_RemovingStarsFromString:

  def removeStars(s: String): String =
    s.foldLeft(List.empty[Char])((res, ch) => if ch == '*' then res.tail else ch +: res).mkString.reverse
