package com.leetcode.cosminci._2300

object _2283_CheckIfNumHasEqualDigitCountToDigitValue:

  def digitCount(num: String): Boolean =
    val counter = num.groupMapReduce(_ - '0')(_ => 1)(_ + _)
    num.indices.forall(i => counter.getOrElse(i, 0) == num(i) - '0')
