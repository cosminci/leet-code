package com.leetcode.cosminci._1000

import scala.collection.mutable

object _983_MinCostForTickets:
  def main(args: Array[String]): Unit =
    List(
      (Array(1, 4, 6, 7, 8, 20), Array(2, 7, 15)),
      (Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 31), Array(2, 7, 15)),
      (Array(1, 30, 60, 90, 91, 92, 93, 94, 95), Array(1, 5, 30))
    ).foreach { case (days, costs) =>
      println(s"Top down: ${mincostTicketsTopDown(days, costs)}")
      println(s"Bottom up: ${mincostTicketsBottomUp(days, costs)}")
    }

  def mincostTicketsBottomUp(days: Array[Int], costs: Array[Int]): Int =
    val dp     = Array.ofDim[Int](days.last + 1)
    val daySet = Set.from(days)
    dp(0) = 0
    (1 to days.last).foreach { day =>
      if !daySet.contains(day) then dp(day) = dp(day - 1)
      else {
        val prev7        = if day - 7 > 0 then dp(day - 7) else dp(0)
        val prev30       = if day - 30 > 0 then dp(day - 30) else dp(0)
        val minWeekMonth = math.min(costs(1) + prev7, costs(2) + prev30)
        dp(day) = math.min(costs(0) + dp(day - 1), minWeekMonth)
      }
    }
    dp.last

  def mincostTicketsTopDown(days: Array[Int], costs: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(dayIdx: Int, validThrough: Int): Int =
      if dayIdx == days.length then return 0
      if mem.contains((dayIdx, validThrough)) then return mem((dayIdx, validThrough))

      if validThrough >= days(dayIdx) then dfs(dayIdx + 1, validThrough)
      else {
        val buyDayPass   = costs(0) + dfs(dayIdx + 1, days(dayIdx))
        val buyWeekPass  = costs(1) + dfs(dayIdx + 1, days(dayIdx) + 6)
        val buyMonthPass = costs(2) + dfs(dayIdx + 1, days(dayIdx) + 29)
        val result       = math.min(buyDayPass, math.min(buyWeekPass, buyMonthPass))
        mem.update((dayIdx, validThrough), result)
        result
      }
    dfs(0, -1)
