package com.leetcode.cosminci._100

import scala.collection.mutable

object _72_EditDistance:

  def minDistance(word1: String, word2: String): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, j: Int): Int = mem.getOrElseUpdate((i, j),
      if i == word1.length then word2.length - j
      else if j == word2.length then word1.length - i
      else if word1(i) == word2(j) then dfs(i + 1, j + 1)
      else (1 + dfs(i + 1, j)).min(1 + dfs(i, j + 1)).min(1 + dfs(i + 1, j + 1))
    )
    dfs(i = 0, j = 0)
