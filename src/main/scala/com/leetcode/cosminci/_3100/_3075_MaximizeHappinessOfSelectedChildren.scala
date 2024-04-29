package com.leetcode.cosminci._3100

object _3075_MaximizeHappinessOfSelectedChildren:

  def maximumHappinessSum(happiness: Array[Int], k: Int): Long =
    happiness.sortBy(v => -v).zipWithIndex
      .takeWhile { case (h, i) => h - i > 0 && i < k }
      .map { case (h, i) => (h - i).toLong }
      .sum
