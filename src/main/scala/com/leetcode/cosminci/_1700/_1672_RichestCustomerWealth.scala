package com.leetcode.cosminci._1700

object _1672_RichestCustomerWealth:
  def maximumWealth(accounts: Array[Array[Int]]): Int = accounts.map(_.sum).max
