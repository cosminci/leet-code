package com.leetcode.cosminci._100

object _10_RegexMatching:

  def main(args: Array[String]): Unit =
    println(isMatch("rrrr", "r*"))

  def isMatch(s: String, p: String): Boolean =
    def isMatchRecursive(sIdx: Int, pIdx: Int): Boolean =
      if pIdx >= p.length then return sIdx == s.length

      val headMatches =
        sIdx < s.length && (s(sIdx) == p(pIdx) || p(pIdx) == '.')

      if pIdx + 1 < p.length && p(pIdx + 1) == '*' then
        headMatches && isMatchRecursive(sIdx + 1, pIdx) || isMatchRecursive(
          sIdx,
          pIdx + 2
        )
      else headMatches && isMatchRecursive(sIdx + 1, pIdx + 1)

    isMatchRecursive(0, 0)
