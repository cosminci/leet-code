package com.leetcode.cosminci._2500

object _2481_MinCutsToDivideCircle:

  def numberOfCuts(n: Int): Int =
    if n == 1 then 0
    else if n % 2 == 1 then n
    else n / 2
