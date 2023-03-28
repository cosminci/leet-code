package com.leetcode.cosminci._1000

import scala.collection.mutable

object _983_MinCostForTickets:

  def mincostTickets(days: Array[Int], costs: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, validThrough: Int): Int = mem.getOrElseUpdate((i, validThrough),
      if i == days.length then 0
      else if validThrough >= days(i) then dfs(i + 1, validThrough)
      else
        val buyDayPass   = costs(0) + dfs(i + 1, days(i))
        val buyWeekPass  = costs(1) + dfs(i + 1, days(i) + 6)
        val buyMonthPass = costs(2) + dfs(i + 1, days(i) + 29)
        buyDayPass.min(buyWeekPass).min(buyMonthPass)
    )
    dfs(0, -1)
