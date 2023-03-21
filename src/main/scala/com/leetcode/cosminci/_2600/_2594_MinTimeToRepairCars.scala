package com.leetcode.cosminci._2600

object _2594_MinTimeToRepairCars:

  def repairCars(ranks: Array[Int], cars: Int): Long =
    @annotation.tailrec
    def dfs(l: Long, r: Long): Long =
      if l >= r then l
      else
        val mid  = l + (r - l) / 2
        val curr = ranks.map(r => math.sqrt(mid.toDouble / r).toLong).sum
        if curr < cars then dfs(mid + 1, r) else dfs(l, mid)

    dfs(l = 1L, r = ranks.head.toLong * cars * cars)
