package com.leetcode.cosminci._2400

object _2335_MinTimeToFillCups:

  def fillCups(amount: Array[Int]): Int =
    amount.max.max((amount.sum + 1) / 2)
