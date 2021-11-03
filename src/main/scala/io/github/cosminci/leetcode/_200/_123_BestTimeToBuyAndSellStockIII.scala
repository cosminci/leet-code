package io.github.cosminci.leetcode._200

object _123_BestTimeToBuyAndSellStockIII {
  def main(args: Array[String]): Unit =
    println(maxProfit(Array(3, 3, 5, 0, 0, 3, 1, 4)))

  def maxProfit(prices: Array[Int]): Int =
    prices.tail.foldLeft(Int.MaxValue, 0, Int.MaxValue, 0) {
      case ((buy1, sell1, buy2, sell2), price) =>
        (math.min(buy1, price),
          math.max(sell1, price - buy1),
          math.min(buy2, price - sell1),
          math.max(sell2, price - buy2))
    }._4
}
