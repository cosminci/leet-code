package com.leetcode.cosminci._2300

object _2220_MinBitFlipsToConvertNumber {
  def minBitFlips(start: Int, goal: Int): Int = {
    def bitCount(n: Int): Int =
      if (n == 0) 0 else n % 2 + bitCount(n / 2)
    bitCount(start ^ goal)
  }
}
