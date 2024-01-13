package com.leetcode.cosminci._3000

object _2957_RemoveAdjacentAlmostEqChars:

  def removeAlmostEqualCharacters(word: String): Int =
    @annotation.tailrec
    def dfs(i: Int, cnt: Int): Int =
      if i >= word.length then cnt
      else if (word(i) - word(i - 1)).abs <= 1 then dfs(i + 2, cnt + 1)
      else dfs(i + 1, cnt)

    dfs(i = 1, cnt = 0)
