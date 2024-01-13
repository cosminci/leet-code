package com.leetcode.cosminci._3000

object _2952_MinCoinsToBeAdded:

  def minimumAddedCoins(coins: Array[Int], target: Int): Int =
    @annotation.tailrec
    def dfs(i: Int, max: Int, additions: Int): Int =
      if max >= target then additions
      else if i < coins.length && coins(i) <= max + 1 then dfs(i + 1, max + coins(i), additions)
      else dfs(i, max + max + 1, additions + 1)

    coins.sortInPlace()
    dfs(i = 0, max = 0, additions = 0)
