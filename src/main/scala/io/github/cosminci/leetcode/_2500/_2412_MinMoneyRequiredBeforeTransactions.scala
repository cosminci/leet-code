package io.github.cosminci.leetcode._2500

object _2412_MinMoneyRequiredBeforeTransactions:

  def minimumMoney(transactions: Array[Array[Int]]): Long =
    transactions.map { case Array(cost, cashback) =>
      (cost.toLong - cashback).max(0L)
    }.sum + transactions.map(_.min).max
