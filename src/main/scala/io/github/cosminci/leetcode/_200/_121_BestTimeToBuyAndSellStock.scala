package io.github.cosminci.leetcode._200

object _121_BestTimeToBuyAndSellStock:
  def main(args: Array[String]): Unit =
    println(maxProfit(Array(7, 1, 5, 3, 6, 4)))
    println(maxProfit(Array(7, 6, 4, 3, 1)))
    println(maxProfit(Array(1)))
    println(maxProfit(Array(2, 4, 1)))

  def maxProfit(prices: Array[Int]): Int =
    prices.foldLeft(0, Int.MaxValue) { case ((maxProfit, minPrice), price) =>
      if (price < minPrice) (maxProfit, price)
      else (maxProfit.max(price - minPrice), minPrice)
    }._1
