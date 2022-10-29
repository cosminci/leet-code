package com.leetcode.cosminci._400

import scala.collection.mutable

object _322_CoinChange:

  def coinChange(coins: Array[Int], amount: Int): Int =
    val mem = mutable.Map.empty[Int, Int]

    def dfs(sum: Int): Int = mem.getOrElseUpdate(sum,
      if sum < 0 then -1
      else if sum == 0 then 0
      else coins
        .map(c => dfs(sum - c))
        .filter(_ >= 0)
        .minOption
        .map(_ + 1)
        .getOrElse(-1)
    )

    dfs(amount)
