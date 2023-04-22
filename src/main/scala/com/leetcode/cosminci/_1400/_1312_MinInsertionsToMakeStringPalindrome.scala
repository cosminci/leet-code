package com.leetcode.cosminci._1400

import scala.collection.mutable

object _1312_MinInsertionsToMakeStringPalindrome:

  def minInsertions(s: String): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, j: Int): Int = mem.getOrElseUpdate((i, j),
      if i >= j then 0
      else if s(i) == s(j) then dfs(i + 1, j - 1)
      else 1 + dfs(i + 1, j).min(dfs(i, j - 1))
    )
    dfs(i = 0, j = s.length - 1)
