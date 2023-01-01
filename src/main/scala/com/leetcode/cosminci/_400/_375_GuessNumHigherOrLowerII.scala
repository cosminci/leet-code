package com.leetcode.cosminci._400

import scala.collection.mutable

object _375_GuessNumHigherOrLowerII:

  def getMoneyAmount(n: Int): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(l: Int, r: Int): Int = mem.getOrElseUpdate((l, r),
      if l >= r then 0
      else (l to r).map(pick => pick + (dfs(l, pick - 1) max dfs(pick + 1, r))).min
    )
    dfs(l = 1, r = n)
