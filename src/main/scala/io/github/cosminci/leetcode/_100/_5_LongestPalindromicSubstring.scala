package io.github.cosminci.leetcode._100

object _5_LongestPalindromicSubstring:

  def longestPalindrome(s: String): String =
    @annotation.tailrec
    def expand(l: Int, r: Int, prev: String): String =
      if l < 0 || r >= s.length || s.charAt(l) != s.charAt(r) then prev
      else expand(l - 1, r + 1, s.substring(l, r + 1))

    s.indices.foldLeft("") { (prevMax, i) =>
      Array(prevMax, expand(l = i, r = i, prev = ""), expand(l = i, r = i + 1, prev = "")).maxBy(_.length)
    }
