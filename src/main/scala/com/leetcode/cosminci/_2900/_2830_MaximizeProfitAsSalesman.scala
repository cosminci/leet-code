package com.leetcode.cosminci._2900

import scala.collection.mutable

object _2830_MaximizeProfitAsSalesman:

  def maximizeTheProfit(n: Int, offers: List[List[Int]]): Int =
    val sorted = offers.map(o => (o(0), o(1), o(2))).sorted

    val mem = mutable.Map.empty[Int, Int]
    def dfs(i: Int): Int = mem.getOrElseUpdate(i,
      if i >= sorted.length then 0
      else
        val (_, end, profit) = sorted(i)
        val next             = sorted.search((end + 1, 0, 0)).insertionPoint
        (profit + dfs(next)).max(dfs(i + 1))
    )

    dfs(i = 0)
