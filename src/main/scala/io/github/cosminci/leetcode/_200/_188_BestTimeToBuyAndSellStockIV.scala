package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _188_BestTimeToBuyAndSellStockIV:

  def maxProfit(k: Int, prices: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int, Boolean), Int]

    def dfs(day: Int, k: Int, canSell: Boolean): Int = mem.getOrElseUpdate((day, k, canSell), {
      if day == prices.length || k == 0 then 0
      else if canSell then dfs(day + 1, k, canSell).max(prices(day) + dfs(day + 1, k - 1, canSell = false))
      else dfs(day + 1, k, canSell).max(-prices(day) + dfs(day + 1, k, canSell = true))
    })

    dfs(day = 0, k, canSell = false)
