package com.leetcode.cosminci._2200

object _2165_SmallestValueOfTheRearrangedNumber:

  def smallestNumber(num: Long): Long =
    val digits = num.abs.toString.map(_ - '0').sorted

    if num < 0 then
      -digits.foldRight(0L)((d, res) => res * 10 + d)
    else
      val firstNonZero = digits.indexWhere(_ > 0)
      Option
        .when(firstNonZero < 1)(digits)
        .getOrElse(digits.updated(0, digits(firstNonZero)).updated(firstNonZero, 0))
        .foldLeft(0L)((res, d) => res * 10 + d)
