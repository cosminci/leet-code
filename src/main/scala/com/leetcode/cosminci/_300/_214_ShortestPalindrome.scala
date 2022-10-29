package com.leetcode.cosminci._300

object _214_ShortestPalindrome:

  def shortestPalindrome(s: String): String =
    val r = s.reverse
    s.indices
      .find(i => s.startsWith(r.substring(i)))
      .map(i => s"${r.substring(0, i)}$s")
      .getOrElse("")
