package io.github.cosminci.leetcode._2200

object _2177_FindThreeConsecIntsThatSumToNum:

  def sumOfThree(num: Long): Array[Long] =
    if num % 3 != 0 then Array.empty
    else Array(num / 3 - 1, num / 3, num / 3 + 1)
