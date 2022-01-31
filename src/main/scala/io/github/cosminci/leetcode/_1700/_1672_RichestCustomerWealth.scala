package io.github.cosminci.leetcode._1700

object _1672_RichestCustomerWealth:
  def maximumWealth(accounts: Array[Array[Int]]): Int = accounts.map(_.sum).max
