package io.github.cosminci.leetcode._1800

import scala.collection.mutable

object _1770_MaxScoreFromMultiplicationOps:

  def maximumScore(nums: Array[Int], multipliers: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(left: Int, op: Int): Int = mem.getOrElseUpdate((left, op),
      if op == multipliers.length then 0
      else
        val right = nums.length - 1 - op + left
        (nums(left) * multipliers(op) + dfs(left + 1, op + 1)).max(nums(right) * multipliers(op) + dfs(left, op + 1))
    )

    dfs(left = 0, op = 0)
