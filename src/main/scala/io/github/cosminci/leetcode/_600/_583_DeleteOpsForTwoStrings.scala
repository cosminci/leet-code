package io.github.cosminci.leetcode._600

import scala.collection.mutable

object _583_DeleteOpsForTwoStrings:
  def main(args: Array[String]): Unit =
    println(minDistanceTopDown("sea", "eat"))
    println(minDistanceBottomUp("sea", "eat"))
    println(minDistanceTopDown("leetcode", "etco"))
    println(minDistanceBottomUp("leetcode", "etco"))

  private def minDistanceTopDown(word1: String, word2: String): Int =
    val (m, n) = (word1.length, word2.length)

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, j: Int): Int =
      mem.getOrElseUpdate(
        (i, j), {
          if i == m then n - j
          else if j == n then m - i
          else if word1(i) == word2(j) then dfs(i + 1, j + 1)
          else 1 + math.min(dfs(i + 1, j), dfs(i, j + 1))
        }
      )

    dfs(i = 0, j = 0)

  private def minDistanceBottomUp(word1: String, word2: String): Int =
    val (m, n) = (word1.length, word2.length)
    val dp     = Array.tabulate(m + 1, n + 1)((i, j) => if i == 0 then j else if j == 0 then i else 0)

    for
      i <- word1.indices
      j <- word2.indices
    do
      dp(i + 1)(j + 1) =
        if word1(i) == word2(j) then dp(i)(j)
        else 1 + math.min(dp(i)(j + 1), dp(i + 1)(j))

    dp(m)(n)
