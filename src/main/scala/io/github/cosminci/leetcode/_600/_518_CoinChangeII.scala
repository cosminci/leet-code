package io.github.cosminci.leetcode._600

import scala.collection.mutable

object _518_CoinChangeII:

  def main(args: Array[String]): Unit =
    println(changeBottomUp(5, Array(1, 2, 5)))
    println(changeTopDown(5, Array(1, 2, 5)))

  private def changeBottomUp(amount: Int, coins: Array[Int]): Int =
    if amount == 0 then return 1
    val dp = Array.ofDim[Int](amount + 1)
    dp(0) = 1
    coins.foreach { c =>
      (1 to amount).foreach { a =>
        if a - c >= 0 then dp(a) += dp(a - c)
      }
    }
    dp.last

  private def changeTopDown(amount: Int, coins: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(cIdx: Int, a: Int): Int =
      if a == 0 then return 1
      if a < 0 || cIdx == coins.length then return 0
      if mem.contains((cIdx, a)) then return mem((cIdx, a))

      val result = dfs(cIdx, a - coins(cIdx)) + dfs(cIdx + 1, a)
      mem.update((cIdx, a), result)
      result
    dfs(0, amount)
