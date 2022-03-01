package io.github.cosminci.leetcode._400

object _338_CountingBits:

  def countBitsBruteForce(n: Int): Array[Int] =
    @annotation.tailrec
    def dfs(n: Int, c: Int): Int =
      if n == 0 then c
      else dfs(n >> 1, c + (n & 1))

    (0 to n).map(dfs(_, c = 0)).toArray
