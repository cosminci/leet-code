package com.leetcode.cosminci._200

object _172_FactorialTrailingZeroes:

  def trailingZeroes(n: Int): Int =
    if n == 0 then 0 else n / 5 + trailingZeroes(n / 5)
