package com.leetcode.cosminci._400

object _326_PowerOfThree {

  def isPowerOfThree(n: Int): Boolean =
    Integer.toString(n, 3).matches("10*")
}
