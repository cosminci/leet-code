package com.leetcode.cosminci._1600

object _1502_CanMakeArithmeticProgressionFromSequence:

  def canMakeArithmeticProgression(arr: Array[Int]): Boolean =
    arr.sorted
      .sliding(2)
      .map { case Array(prev, curr) => curr - prev }
      .distinct
      .size == 1
