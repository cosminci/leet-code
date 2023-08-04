package com.leetcode.cosminci._200

import scala.collection.mutable

object _139_WordBreak:

  def wordBreak(input: String, wordDict: List[String]): Boolean =
    val dict = wordDict.toSet

    val mem = mutable.Map.empty[String, Boolean]
    def dfs(s: String): Boolean = mem.getOrElseUpdate(s,
      if s.isEmpty then false
      else if dict.contains(s) then return true
      else dict.exists(w => s.startsWith(w) && dfs(s.substring(w.length)))
    )

    dfs(input)
