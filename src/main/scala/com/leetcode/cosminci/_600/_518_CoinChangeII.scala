package com.leetcode.cosminci._600

import scala.collection.mutable

object _518_CoinChangeII:

  def change(amount: Int, coins: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, amount: Int): Int = mem.getOrElseUpdate((i, amount),
      if amount == 0 then 1
      else if amount < 0 || i == coins.length then 0
      else dfs(i, amount - coins(i)) + dfs(i + 1, amount)
    )
    dfs(i = 0, amount)
