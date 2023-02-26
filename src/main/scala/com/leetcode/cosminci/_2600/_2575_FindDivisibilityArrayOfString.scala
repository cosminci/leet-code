package com.leetcode.cosminci._2600

object _2575_FindDivisibilityArrayOfString:

  def divisibilityArray(word: String, m: Int): Array[Int] =
    word
      .map(_ - '0')
      .scanLeft(0L)((acc, v) => (acc * 10 + v) % m).tail
      .map(v => if v == 0 then 1 else 0).toArray
