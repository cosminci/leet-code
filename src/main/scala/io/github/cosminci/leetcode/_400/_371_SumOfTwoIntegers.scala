package io.github.cosminci.leetcode._400

object _371_SumOfTwoIntegers:
  def getSum(a: Int, b: Int): Int =
    if b == 0 then return a
    getSum(a ^ b, (a & b) << 1)
