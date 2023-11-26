package com.leetcode.cosminci._3000

import scala.collection.mutable

object _2944_MinCoinsForFruits:

  def minimumCoins(prices: Array[Int]): Int =
    val mem = mutable.Map.empty[Int, Int]
    def dfs(i: Int): Int = mem.getOrElseUpdate(i,
      if i >= prices.length then 0
      else prices(i) + (i + 1 to (2 * i + 2).min(prices.length)).map(dfs).min
    )
    dfs(i = 0)
