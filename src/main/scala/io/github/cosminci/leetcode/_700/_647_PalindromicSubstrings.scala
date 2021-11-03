package io.github.cosminci.leetcode._700

object _647_PalindromicSubstrings:

  def main(args: Array[String]): Unit =
    println(countSubstrings("abc"))
    println(countSubstrings("aaa"))

  def countSubstrings(s: String): Int =
    (1 until s.length).foldLeft(1) { case (acc, origin) =>
      acc +
        count(s, left = origin, right = origin) +
        count(s, left = origin - 1, right = origin)
    }

  def count(s: String, left: Int, right: Int): Int =
    var (result, l, r) = (0, left, right)
    while l >= 0 && r <= s.length - 1 && s(l) == s(r) do
      l -= 1
      r += 1
      result += 1
    result
