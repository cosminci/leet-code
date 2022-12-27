package com.leetcode.cosminci._100

import scala.collection.mutable

object _87_ScrambleString:

  def isScramble(s1: String, s2: String): Boolean =
    val mem = mutable.Map.empty[(String, String), Boolean]
    def dfs(s1: String, s2: String): Boolean = mem.getOrElseUpdate((s1, s2),
      if s1 == s2 then true
      else
        (1 until s1.length).exists { i =>
          val n = s1.length
          (dfs(s1.slice(0, i), s2.slice(0, i)) && dfs(s1.slice(i, n), s2.slice(i, n))) ||
          (dfs(s1.slice(0, i), s2.slice(n - i, n)) && dfs(s1.slice(i, n), s2.slice(0, n - i)))
        }
    )
    s1.toSet == s2.toSet && dfs(s1, s2)
