package com.leetcode.cosminci._100

import scala.collection.mutable

object _91_DecodeWays:

  def numDecodings(s: String): Int =
    val mem = mutable.Map.empty[Int, Int]
    def dfs(i: Int): Int = mem.getOrElseUpdate(i, {
      if i == s.length then 1
      else if s(i) == '0' then 0
      else if i == s.length - 1 || s"${s(i)}${s(i + 1)}".toInt > 26 then dfs(i + 1)
      else dfs(i + 1) + dfs(i + 2)
    })

    dfs(i = 0)
