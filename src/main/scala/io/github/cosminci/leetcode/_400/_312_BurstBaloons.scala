package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _312_BurstBaloons:
  def main(args: Array[String]): Unit =
    println(maxCoins(Array(3, 1, 5, 8)))
    println(maxCoins(Array(1, 5)))
    println(maxCoins(Array(1)))

  private def maxCoins(nums: Array[Int]): Int =
    val numbers = nums.prepended(1).appended(1)
    val mem     = mutable.Map.empty[(Int, Int), Int]

    def dfs(l: Int, r: Int): Int =
      if l > r then return 0
      if mem.contains((l, r)) then return mem((l, r))
      val result = (l to r).map { i =>
        numbers(l - 1) * numbers(i) * numbers(r + 1) +
          dfs(l, i - 1) + dfs(i + 1, r)
      }.max
      mem.update((l, r), result)
      result
    dfs(1, nums.length)
