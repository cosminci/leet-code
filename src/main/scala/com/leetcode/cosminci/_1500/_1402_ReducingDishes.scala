package com.leetcode.cosminci._1500

import scala.collection.mutable

object _1402_ReducingDishes:

  def maxSatisfaction(satisfaction: Array[Int]): Int =
    satisfaction.sortInPlace()
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, time: Int): Int = mem.getOrElseUpdate((i, time),
      if i == satisfaction.length then 0
      else (satisfaction(i) * (time + 1) + dfs(i + 1, time + 1)).max(dfs(i + 1, time))
    )
    dfs(i = 0, time = 0)
