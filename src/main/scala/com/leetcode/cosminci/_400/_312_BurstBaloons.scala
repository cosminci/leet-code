package com.leetcode.cosminci._400

import scala.collection.mutable

object _312_BurstBaloons:
  def main(args: Array[String]): Unit =
    println(maxCoins(Array(3, 1, 5, 8)))
    println(maxCoins(Array(1, 5)))
    println(maxCoins(Array(1)))

  def maxCoins(nums: Array[Int]): Int = {
    val numbers = 1 +: nums :+ 1

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(l: Int, r: Int): Int = mem.getOrElseUpdate((l, r), {
      if (l > r) 0
      else (l to r).map { i =>
        numbers(l - 1) * numbers(i) * numbers(r + 1) +
          dfs(l, i - 1) + dfs(i + 1, r)
      }.max
    })

    dfs(1, nums.length)
  }


