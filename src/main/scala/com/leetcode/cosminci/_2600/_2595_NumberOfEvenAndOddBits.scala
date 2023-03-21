package com.leetcode.cosminci._2600

object _2595_NumberOfEvenAndOddBits:

  def evenOddBit(n: Int): Array[Int] =
    n.toBinaryString.reverse.zipWithIndex.foldLeft(Array(0, 0)) { case (result, (n, i)) =>
      if n == '0' then result
      else result.updated(i % 2, result(i % 2) + 1)
    }
