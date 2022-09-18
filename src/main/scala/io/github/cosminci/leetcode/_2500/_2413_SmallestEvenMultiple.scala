package io.github.cosminci.leetcode._2500

object _2413_SmallestEvenMultiple:

  def smallestEvenMultiple(n: Int): Int =
    if n % 2 == 0 then n else n * 2
