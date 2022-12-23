package com.leetcode.cosminci._400

import scala.util.chaining.*

object _309_BestTimeToBuyAndSellStockWithCooldown:

  def maxProfit(prices: Array[Int]): Int =
    prices.tail
      .foldLeft(0, -prices.head, Int.MinValue) { case ((prevCanBuy, prevCanSell, prevMustRest), price) =>
        (prevCanBuy max prevMustRest, prevCanSell max (prevCanBuy - price), prevCanSell + price)
      }
      .pipe { case (canBuy, _, mustRest) => canBuy max mustRest }
