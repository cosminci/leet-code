package com.leetcode.cosminci._400

object _342_PowerOfFour:

  def isPowerOfFour(n: Int): Boolean =
    n > 0 && (n & (n - 1)) == 0 && (n & 0x55555555) != 0
