package com.leetcode.cosminci._2500

import scala.collection.mutable

object _2486_AppendCharsToStringToMakeSubseq:

  def appendCharacters(s: String, t: String): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, j: Int): Int = mem.getOrElseUpdate((i, j),
      if j == t.length then 0
      else if i == s.length then t.length - j
      else if s(i) == t(j) then dfs(i + 1, j + 1)
      else dfs(i + 1, j)
    )
    dfs(i = 0, j = 0)
