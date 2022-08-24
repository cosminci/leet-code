package io.github.cosminci.leetcode._400

object _326_PowerOfThree {

  def isPowerOfThree(n: Int): Boolean =
    Integer.toString(n, 3).matches("10*")
}
