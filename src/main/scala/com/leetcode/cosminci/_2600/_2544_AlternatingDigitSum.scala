package com.leetcode.cosminci._2600

object _2544_AlternatingDigitSum:

  def alternateDigitSum(n: Int): Int =
    n.toString.zipWithIndex.map { case (ch, i) => if i % 2 == 0 then ch - '0' else '0' - ch }.sum
