package com.leetcode.cosminci._400

object _392_IsSubsequence:

  def isSubsequenceRecursive(s: String, t: String): Boolean =
    @annotation.tailrec
    def dfs(sIdx: Int, tIdx: Int): Boolean =
      if sIdx == s.length then true
      else if tIdx == t.length then false
      else if s(sIdx) == t(tIdx) then dfs(sIdx + 1, tIdx + 1)
      else dfs(sIdx, tIdx + 1)

    dfs(sIdx = 0, tIdx = 0)
