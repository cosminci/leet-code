package com.leetcode.cosminci._3000

object _2939_MaxXorProduct:

  def maximumXorProduct(a: Long, b: Long, n: Int): Int =
    val mod = 1_000_000_007

    @annotation.tailrec
    def dfs(a: Long, b: Long, bit: Long): Long =
      if n == 0 || bit == 0 then ((a % mod) * (b % mod)) % mod
      else if (a.min(b) & bit) > 0 then dfs(a, b, bit >> 1)
      else dfs(a ^ bit, b ^ bit, bit >> 1)

    dfs(a, b, 1L << (n - 1)).toInt
