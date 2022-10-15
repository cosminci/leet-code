package io.github.cosminci.leetcode._1600

import scala.collection.mutable

object _1531_StringCompressionII:

  def getLengthOfOptimalCompression(s: String, k: Int): Int =
    val mem = mutable.Map.empty[(Int, Char, Int, Int), Int]
    def dfs(i: Int, prev: Char, len: Int, k: Int): Int = mem.getOrElseUpdate(
      (i, prev, len, k),
      if k < 0 then s.length
      else if i == s.length then 0
      else if s(i) != prev then (1 + dfs(i + 1, s(i), len = 1, k)).min(dfs(i + 1, prev, len, k - 1))
      else Option.when(Array(1, 9, 99).contains(len))(1).getOrElse(0) + dfs(i + 1, prev, len + 1, k)
    )
    dfs(i = 0, prev = '~', len = 0, k)
