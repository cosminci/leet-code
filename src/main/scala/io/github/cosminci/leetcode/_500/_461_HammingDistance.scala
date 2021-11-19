package io.github.cosminci.leetcode._500

object _461_HammingDistance {
  def hammingDistance(x: Int, y: Int): Int = {
    def dfs(n: Int): Int =
      Option.when(n > 0)((n & 1) + dfs(n >> 1)).getOrElse(0)

    dfs(x ^ y)
  }
}
