package com.leetcode.cosminci._2900

object _2864_MaxOddBinaryNum:

  def maximumOddBinaryNumber(s: String): String =
    val (zeroes, ones) = s.partitionMap(c => if c == '0' then Left(c) else Right(c))
    ones.tail ++ zeroes ++ "1"
