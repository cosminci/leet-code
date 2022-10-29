package com.leetcode.cosminci._1100

object _1015_SmallestIntegerDivisibleByK {
  def smallestRepunitDivByK(k: Int): Int =
    (1 to k).foldLeft(0) { (remainder, length) =>
      val newRemainder = (remainder * 10 + 1) % k
      if (newRemainder == 0) return length
      newRemainder
    }
    -1
}
