package com.leetcode.cosminci._2900

object _2806_AccountBalanceAfterRoundedPurchase:

  def accountBalanceAfterPurchase(purchaseAmount: Int): Int =
    100 - (purchaseAmount + 5) / 10 * 10
