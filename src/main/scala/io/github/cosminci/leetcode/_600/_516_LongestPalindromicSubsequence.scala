package io.github.cosminci.leetcode._600

import scala.collection.mutable

object _516_LongestPalindromicSubsequence:
  def main(args: Array[String]): Unit =
    println(longestPalindromeSubseqTopDown("AABBCA"))
    println(longestPalindromeSubseqBottomUp("AABBCA"))

  def longestPalindromeSubseqBottomUp(s: String): Int =
    val dp = Array.ofDim[Int](s.length, s.length)
    s.indices.foreach(i => dp(i)(i) = 1)
    (1 until s.length).foreach { end =>
      (end - 1 to 0 by -1).foreach { start =>
        dp(start)(end) =
          if s(start) == s(end) then if end - start >= 2 then 2 + dp(start + 1)(end - 1) else 2
          else math.max(dp(start)(end - 1), dp(start + 1)(end))
      }
    }
    dp.head.last

  def longestPalindromeSubseqTopDown(s: String): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(l: Int, r: Int): Int =
      if l == r then return 1
      if r - l == 1 && s(l) == s(r) then return 2
      if mem.contains((l, r)) then return mem((l, r))

      val result =
        if s(l) == s(r) then 2 + dfs(l + 1, r - 1)
        else math.max(dfs(l + 1, r), dfs(l, r - 1))

      mem.update((l, r), result)
      result

    dfs(0, s.length - 1)
