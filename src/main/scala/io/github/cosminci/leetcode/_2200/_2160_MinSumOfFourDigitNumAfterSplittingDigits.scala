package io.github.cosminci.leetcode._2200

object _2160_MinSumOfFourDigitNumAfterSplittingDigits:

  def minimumSum(num: Int): Int =
    val digits = num.toString.map(_ - '0').sorted
    10 * digits(0) + digits(2) + 10 * digits(1) + digits(3)
