package com.leetcode.cosminci._200

import scala.collection.mutable

object _115_DistinctSubsequences:
  def main(args: Array[String]): Unit =
    println(numDistinctTopDown("raabbbit", "rabbit"))
    println(numDistinctBottomUp("raabbbit", "rabbit"))
    println(numDistinctTopDown("babgbag", "bag"))
    println(numDistinctBottomUp("babgbag", "bag"))

  def numDistinctBottomUp(s: String, t: String): Int =
    val dp = Array.ofDim[Int](s.length + 1, t.length + 1)
    dp.indices.foreach(i => dp(i)(0) = 1)
    t.indices.foreach { tIdx =>
      s.indices.foreach { sIdx =>
        dp(sIdx + 1)(tIdx + 1) =
          if s(sIdx) == t(tIdx) then dp(sIdx)(tIdx) + dp(sIdx)(tIdx + 1)
          else dp(sIdx)(tIdx + 1)
      }
    }
    dp(s.length)(t.length)

  def numDistinctTopDown(s: String, t: String): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(sIdx: Int, tIdx: Int): Int =
      if tIdx == t.length then return 1
      if sIdx == s.length then return 0
      if mem.contains((sIdx, tIdx)) then return mem((sIdx, tIdx))

      val result =
        if s(sIdx) == t(tIdx) then dfs(sIdx + 1, tIdx + 1) + dfs(sIdx + 1, tIdx)
        else dfs(sIdx + 1, tIdx)

      mem.update((sIdx, tIdx), result)
      result
    dfs(0, 0)
