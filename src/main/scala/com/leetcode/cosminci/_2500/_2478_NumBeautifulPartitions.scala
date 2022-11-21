package com.leetcode.cosminci._2500

import scala.collection.mutable

object _2478_NumBeautifulPartitions:

  def beautifulPartitions(s: String, k: Int, minLength: Int): Int =
    def prime(ch: Char)        = "2357".contains(ch)
    def jump(atStart: Boolean) = if atStart then minLength - 1 else 1

    val mem = mutable.Map.empty[(Int, Boolean, Int), Long]
    def dfs(i: Int, atStart: Boolean, k: Int): Long = mem.getOrElseUpdate((i, atStart, k),
      if i == s.length then if k == 0 then 1 else 0
      else if i > s.length || k == 0 || (!prime(s(i)) && atStart) then 0
      else if prime(s(i)) then dfs(i + jump(atStart), atStart = false, k)
      else (dfs(i + 1, atStart = true, k - 1) + dfs(i + 1, atStart = false, k)) % 1_000_000_007
    )

    dfs(i = 0, atStart = true, k).toInt
