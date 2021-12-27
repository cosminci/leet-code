package io.github.cosminci.leetcode._500

object _476_NumberComplement {
  def findComplement(num: Int): Int =
    ~num & ((Integer.highestOneBit(num) << 1) - 1)
}
