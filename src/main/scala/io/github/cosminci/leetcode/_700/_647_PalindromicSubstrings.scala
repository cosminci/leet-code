package io.github.cosminci.leetcode._700

object _647_PalindromicSubstrings:

  def main(args: Array[String]): Unit =
    println(countSubstrings("abc"))
    println(countSubstrings("aaa"))

  def countSubstrings(s: String): Int =
    @annotation.tailrec
    def dfs(l: Int, r: Int, cnt: Int): Int =
      if l < 0 || r == s.length || s(l) != s(r) then cnt
      else dfs(l - 1, r + 1, cnt + 1)

    (1 until s.length).map { i =>
      dfs(l = i, r = i, cnt = 0) + dfs(l = i - 1, r = i, cnt = 0)
    }.sum + 1
