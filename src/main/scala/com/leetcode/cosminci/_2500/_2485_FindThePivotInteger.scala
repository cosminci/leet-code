package com.leetcode.cosminci._2500

object _2485_FindThePivotInteger:

  def pivotInteger(n: Int): Int =
    val sum  = n * (n + 1) / 2
    val sqrt = math.sqrt(sum).toInt
    if sqrt * sqrt == sum then sqrt else -1
