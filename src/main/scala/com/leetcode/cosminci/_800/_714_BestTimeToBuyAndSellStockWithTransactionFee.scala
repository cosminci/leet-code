package com.leetcode.cosminci._800

import scala.collection.mutable

object _714_BestTimeToBuyAndSellStockWithTransactionFee:
  def main(args: Array[String]): Unit =
    println(maxProfit(Array(1, 3, 2, 8, 4, 9), 2))
    println(maxProfit(Array(1, 3, 7, 5, 10, 3), 3))

  def maxProfit(prices: Array[Int], fee: Int): Int =
    prices.tail
      .foldLeft(0, -prices.head) { case ((sold, bought), price) =>
        (math.max(bought + price - fee, sold), math.max(bought, sold - price))
      }._1
