package io.github.cosminci.leetcode._200

object _121_BestTimeToBuyAndSellStock:
  def main(args: Array[String]): Unit =
    println(maxProfit(Array(7, 1, 5, 3, 6, 4)))
    println(maxProfit(Array(7, 6, 4, 3, 1)))
    println(maxProfit(Array(1)))
    println(maxProfit(Array(2, 4, 1)))

  def maxProfit(prices: Array[Int]): Int =
    if prices.length == 1 then return 0
    var maxProfit = 0
    var minPrice  = Int.MaxValue
    prices.indices.foreach { i =>
      val price = prices(i)
      if price < minPrice then minPrice = price
      else maxProfit = math.max(maxProfit, price - minPrice)
    }
    maxProfit
