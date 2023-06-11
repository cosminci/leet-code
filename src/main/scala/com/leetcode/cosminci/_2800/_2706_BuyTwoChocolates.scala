package com.leetcode.cosminci._2800

object _2706_BuyTwoChocolates:

  def buyChoco(prices: Array[Int], money: Int): Int =
    if prices.length < 2 then money
    else
      val left = money - prices.sorted.take(2).sum
      if left >= 0 then left else money
