package io.github.cosminci.leetcode._2200

object _2124_CheckIfAllAsAppearBeforeAllBs:
  def checkString(s: String): Boolean =
    @annotation.tailrec
    def dfs(i: Int, bFound: Boolean): Boolean =
      if i == s.length then true
      else if s(i) == 'a' && bFound then false
      else if s(i) == 'b' then dfs(i + 1, bFound = true)
      else dfs(i + 1, bFound)

    dfs(i = 0, bFound = false)
