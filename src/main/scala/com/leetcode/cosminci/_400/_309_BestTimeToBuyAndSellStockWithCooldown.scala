package com.leetcode.cosminci._400

import scala.collection.mutable

object _309_BestTimeToBuyAndSellStockWithCooldown:
  def main(args: Array[String]): Unit =
    println(maxProfit(Array(1, 2, 3, 0, 2)))

  def maxProfit(prices: Array[Int]): Int =
    val (canBuy, _, mustRest) = prices.tail.foldLeft(0, -prices.head, Int.MinValue) {
      case ((prevCanBuy, prevCanSell, prevMustRest), price) =>
        (math.max(prevCanBuy, prevMustRest), math.max(prevCanSell, prevCanBuy - price), prevCanSell + price)
    }
    math.max(canBuy, mustRest)
