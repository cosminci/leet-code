package com.leetcode.cosminci._100

import scala.collection.mutable

object _44_WildcardMatching:

  def isMatch(s: String, p: String): Boolean =
    val mem = mutable.Map.empty[(Int, Int), Boolean]
    def dfs(i: Int, j: Int): Boolean = mem.getOrElseUpdate((i, j), {
      if i == s.length then j == p.length || p.substring(j).forall(_ == '*')
      else if j == p.length then i == s.length
      else if p(j) == '?' then dfs(i + 1, j + 1)
      else if p(j) == '*' then dfs(i + 1, j) || dfs(i, j + 1) || dfs(i + 1, j + 1)
      else s(i) == p(j) && dfs(i + 1, j + 1)
    })
    dfs(i = 0, j = 0)
