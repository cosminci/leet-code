package io.github.cosminci.leetcode._1200

import scala.collection.mutable

object _1143_LongestCommonSubsequence:

  def main(args: Array[String]): Unit =
    println(longestCommonSubsequenceBottomUp("abcde", "ace"))
    println(longestCommonSubsequenceTopDown("abcde", "ace"))

  private def longestCommonSubsequenceBottomUp(t1: String, t2: String): Int =
    val dp = Array.ofDim[Int](t1.length + 1, t2.length + 1)
    t1.indices.foreach { i =>
      t2.indices.foreach { j =>
        if t1(i) == t2(j) then dp(i + 1)(j + 1) = 1 + dp(i)(j)
        else dp(i + 1)(j + 1) = math.max(dp(i)(j + 1), dp(i + 1)(j))
      }
    }
    dp(t1.length)(t2.length)

  private def longestCommonSubsequenceTopDown(t1: String, t2: String): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(t1Idx: Int, t2Idx: Int): Int =
      if t1Idx == t1.length || t2Idx == t2.length then return 0
      if mem.contains((t1Idx, t2Idx)) then return mem((t1Idx, t2Idx))

      val result =
        if t1(t1Idx) == t2(t2Idx) then dfs(t1Idx + 1, t2Idx + 1) + 1
        else math.max(dfs(t1Idx, t2Idx + 1), dfs(t1Idx + 1, t2Idx))

      mem.update((t1Idx, t2Idx), result)
      result

    dfs(0, 0)
