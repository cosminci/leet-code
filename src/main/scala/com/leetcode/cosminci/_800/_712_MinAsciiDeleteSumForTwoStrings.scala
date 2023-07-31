package com.leetcode.cosminci._800

import scala.collection.mutable

object _712_MinAsciiDeleteSumForTwoStrings:

  def minimumDeleteSum(s1: String, s2: String): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, j: Int): Int =
      mem.getOrElseUpdate((i, j),
        if i == s1.length && j == s2.length then 0
        else if i == s1.length then s2.substring(j).sum
        else if j == s2.length then s1.substring(i).sum
        else if s1(i) == s2(j) then dfs(i + 1, j + 1)
        else (s1(i) + dfs(i + 1, j)).min(s2(j) + dfs(i, j + 1))
      )
    dfs(i = 0, j = 0)
