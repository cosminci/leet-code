package com.leetcode.cosminci._1100

object _1009_ComplementOfBased10Integer:
  def bitwiseComplement(n: Int): Int =
    @annotation.tailrec
    def dfs(mask: Int): Int =
      if mask >= n then mask
      else dfs((mask << 1) + 1)
    dfs(mask = 1) ^ n
