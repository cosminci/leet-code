package com.leetcode.cosminci._1600

object _1523_CountOddNumsInIntervalRange:

  def countOdds(low: Int, high: Int): Int =
    (high + 1) / 2 - low / 2
