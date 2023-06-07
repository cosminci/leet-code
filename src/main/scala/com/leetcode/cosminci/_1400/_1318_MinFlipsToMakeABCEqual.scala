package com.leetcode.cosminci._1400

object _1318_MinFlipsToMakeABCEqual:

  def minFlips(a: Int, b: Int, c: Int): Int =
    Integer.bitCount((a | b) ^ c) + Integer.bitCount(a & b & ~c)
