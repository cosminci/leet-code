package com.leetcode.cosminci._1700

object _1680_ConcatenationOfConsecBinaryNums:

  def concatenatedBinary(n: Int): Int =
    (1 to n).foldLeft(0) { (res, num) =>
      num.toBinaryString.foldLeft(res) { (res, d) =>
        ((res << 1) + d - '0') % 1_000_000_007
      }
    }
