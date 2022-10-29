package com.leetcode.cosminci._100

import scala.collection.mutable

object _72_EditDistance:

  def main(args: Array[String]): Unit =
    println(minDistanceTopDown("intention", "execution"))
    println(minDistanceBottomUp("intention", "execution"))
    println(minDistanceTopDown("horse", "ros"))
    println(minDistanceBottomUp("horse", "ros"))

  def minDistanceBottomUp(word1: String, word2: String): Int =
    val dp = Array.ofDim[Int](word1.length + 1, word2.length + 1)
    (1 to word1.length).foreach { i => dp(i)(0) = i }
    (1 to word2.length).foreach { i => dp(0)(i) = i }

    word1.indices.foreach { c1Idx =>
      word2.indices.foreach { c2Idx =>
        dp(c1Idx + 1)(c2Idx + 1) =
          if word1(c1Idx) == word2(c2Idx) then dp(c1Idx)(c2Idx)
          else
            1 + math.min(
              dp(c1Idx)(c2Idx),
              math.min(dp(c1Idx + 1)(c2Idx), dp(c1Idx)(c2Idx + 1))
            )
      }
    }
    dp(word1.length)(word2.length)

  def minDistanceTopDown(word1: String, word2: String): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(idx1: Int, idx2: Int): Int =
      if idx1 == word1.length then return word2.length - idx2
      if idx2 == word2.length then return word1.length - idx1
      if mem.contains((idx1, idx2)) then return mem((idx1, idx2))

      val result =
        if word1(idx1) == word2(idx2) then dfs(idx1 + 1, idx2 + 1)
        else
          math.min(
            1 + dfs(idx1 + 1, idx2),
            math.min(
              1 + dfs(idx1, idx2 + 1),
              1 + dfs(idx1 + 1, idx2 + 1)
            )
          )

      mem.update((idx1, idx2), result)
      result
    dfs(0, 0)
