package com.leetcode.cosminci._3000

object _2931_MaxSpendingAfterBuyingItems:

  def maxSpending(values: Array[Array[Int]]): Long =
    values.flatten.sorted
      .zip(1 to values.length * values.head.length)
      .map { case (v, day) => v.toLong * day }
      .sum
