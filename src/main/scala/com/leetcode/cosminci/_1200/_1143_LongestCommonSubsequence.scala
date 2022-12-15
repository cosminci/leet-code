package com.leetcode.cosminci._1200

import scala.collection.mutable

object _1143_LongestCommonSubsequence:

  def longestCommonSubsequence(t1: String, t2: String): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, j: Int): Int = mem.getOrElseUpdate((i, j),
      if i == t1.length || j == t2.length then 0
      else if t1(i) == t2(j) then dfs(i + 1, j + 1) + 1
      else dfs(i, j + 1) max dfs(i + 1, j)
    )
    dfs(i = 0, j = 0)
