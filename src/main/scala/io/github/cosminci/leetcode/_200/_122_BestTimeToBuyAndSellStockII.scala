package io.github.cosminci.leetcode._200

object _122_BestTimeToBuyAndSellStockII:
  def main(args: Array[String]): Unit =
    println(maxProfit(Array(1, 2)))

  private def maxProfit(prices: Array[Int]): Int =
    prices.indices.tail.foldLeft(0) { case (profit, i) =>
      if prices(i - 1) < prices(i) then profit + prices(i) - prices(i - 1)
      else profit
    }
