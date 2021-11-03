package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _322_CoinChange:
  def main(args: Array[String]): Unit =
    List(
      (Array(1, 2, 5), 11),
      (Array(2), 3),
      (Array(1), 0),
      (Array(1), 1),
      (Array(1), 2)
    ).foreach { case (coins, amount) =>
      println(s"TopDown: ${coinChangeTopDown(coins, amount)}")
      println(s"BottomUp: ${coinChangeBottomUp(coins, amount)}")
    }

  def coinChangeBottomUp(coins: Array[Int], amount: Int): Int =
    val dp = Array.fill[Int](amount + 1)(amount + 1)
    dp(0) = 0
    (1 to amount).foreach { a =>
      coins.foreach { c =>
        if a - c >= 0 then dp(a) = math.min(dp(a), dp(a - c) + 1)
      }
    }
    if dp.last != amount + 1 then dp.last else -1

  def coinChangeTopDown(coins: Array[Int], amount: Int): Int =
    if amount == 0 then return 0

    val mem = mutable.Map.empty[Int, Int]
    def dfs(sum: Int): Int =
      if sum == 0 then return 0
      if sum < 0 then return -1
      if mem.contains(sum) then return mem(sum)

      val result = coins
        .collect { case c if dfs(sum - c) >= 0 => dfs(sum - c) }
        .minOption
        .map(_ + 1)
        .getOrElse(-1)

      mem.update(sum, result)
      result
    dfs(amount)
