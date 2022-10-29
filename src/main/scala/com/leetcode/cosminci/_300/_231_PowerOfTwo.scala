package com.leetcode.cosminci._300

object _231_PowerOfTwo {
  def isPowerOfTwo(n: Int): Boolean = n > 0 && (n & (n - 1)) == 0
}
