package io.github.cosminci.leetcode._300

object _258_AddDigits:

  def addDigits(num: Int): Int =
    @annotation.tailrec
    def dfs(num: Int, root: Int): Int =
      if num == 0 && root <= 9 then root
      else if num == 0 then dfs(root, 0)
      else dfs(num / 10, root + num % 10)

    dfs(num, 0)
