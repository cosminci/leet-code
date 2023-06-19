package com.leetcode.cosminci._2800

object _2729_CheckIfNumIsFascinating:

  def isFascinating(n: Int): Boolean =
    s"$n${n * 2}${n * 3}".sorted == "123456789"
