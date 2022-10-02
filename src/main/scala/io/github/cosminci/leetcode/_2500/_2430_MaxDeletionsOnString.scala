package io.github.cosminci.leetcode._2500

import scala.collection.mutable

object _2430_MaxDeletionsOnString:

  def deleteString(s: String): Int =
    val mem = mutable.Map.empty[Int, Int]

    def same(i: Int, j: Int) = s.slice(i, i + j) == s.slice(i + j, i + 2 * j)
    def dfs(i: Int): Int = mem.getOrElseUpdate(i,
      if i == s.length then 0
      else Iterator
        .iterate((1, 1)) { case (ans, j) => (if same(i, j) then ans.max(1 + dfs(i + j)) else ans, j + 1) }
        .dropWhile { case (ans, j) => i + 2 * j <= s.length && s.length - i - j + 1 > ans }
        .next()._1
    )

    dfs(i = 0)
