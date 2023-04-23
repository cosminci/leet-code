package com.leetcode.cosminci._1500

import scala.collection.mutable
import scala.util.chaining.*

object _1416_RestoreTheArray:

  def numberOfArrays(s: String, k: Int): Int =
    val mod = 1_000_000_007
    val mem = mutable.Map.empty[Int, Int]

    def dfs(i: Int): Int = mem.getOrElseUpdate(i,
      if i == s.length then 1
      else if s(i) == '0' then 0
      else Iterator
        .iterate((0L, 1, 0L)) { case (res, sz, num) =>
          val newNum = num * 10 + s.charAt(i + sz - 1) - '0'
          val newRes = if newNum <= k then (res + dfs(i + sz)) % mod else res
          (newRes, sz + 1, newNum)
        }
        .dropWhile { case (_, sz, num) => num <= k && i + sz <= s.length }
        .next()
        .pipe { case (res, _, _) => res.toInt }
    )

    dfs(i = 0)
