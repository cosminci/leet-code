package com.leetcode.cosminci._1100

object _1047_RemoveAllAdjDuplicatesInString:

  def removeDuplicates(s: String): String =
    @annotation.tailrec
    def dfs(i: Int, stack: Array[Char]): String =
      if i == s.length then stack.mkString
      else
        stack.lastOption match
          case Some(ch) if ch == s(i) => dfs(i + 1, stack.dropRight(1))
          case _                      => dfs(i + 1, stack :+ s(i))

    dfs(i = 0, stack = Array.empty)
