package com.leetcode.cosminci._2600

import scala.collection.mutable

object _2547_MinCostToSplitArray:

  def minCost(nums: Array[Int], k: Int): Int =
    val mem = mutable.Map.empty[Int, Long]
    def dfs(i: Int): Long = mem.getOrElseUpdate(i,
      if i == nums.length then 0
      else (i until nums.length)
        .foldLeft(Map.empty[Int, Int].withDefaultValue(0), 0L, Int.MaxValue.toLong) {
          case ((counts, trim, prevMin), j) =>
            val newCount  = counts(nums(j)) + 1
            val newCounts = counts.updated(nums(j), newCount)
            val newTrim = trim + (if newCount == 1 then 0 else if newCount == 2 then 2 else 1)
            (newCounts, newTrim, prevMin.min(newTrim + k + dfs(j + 1)))
        }._3
    )
    dfs(i = 0).toInt
