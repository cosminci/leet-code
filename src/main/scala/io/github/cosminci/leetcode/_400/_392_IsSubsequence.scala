package io.github.cosminci.leetcode._400

import scala.annotation.tailrec

object _392_IsSubsequence:
  def main(args: Array[String]): Unit =
    println(isSubsequenceRecursive("abc", "ahbgdc"))
    println(isSubsequenceLoop("abc", "ahbgdc"))

  private def isSubsequenceRecursive(s: String, t: String): Boolean =
    @tailrec
    def dfs(sIdx: Int, tIdx: Int): Boolean =
      if sIdx == s.length then return true
      if tIdx == t.length then return false
      if s(sIdx) == t(tIdx) then dfs(sIdx + 1, tIdx + 1) else dfs(sIdx, tIdx + 1)
    dfs(0, 0)

  private def isSubsequenceLoop(s: String, t: String): Boolean =
    var (sIdx, tIdx) = (0, 0)
    while sIdx != s.length do
      if tIdx == t.length then return false
      if s(sIdx) == t(tIdx) then sIdx += 1
      tIdx += 1
    true
