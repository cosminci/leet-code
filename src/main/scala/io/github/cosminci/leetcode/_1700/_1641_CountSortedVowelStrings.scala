package io.github.cosminci.leetcode._1700

object _1641_CountSortedVowelStrings:

  def countVowelStrings(n: Int): Int =
    def dfs(n: Int): Array[Int] =
      if n == 1 then Array(1, 1, 1, 1, 1)
      else dfs(n - 1).scanLeft(0)(_ + _)

    dfs(n).sum
