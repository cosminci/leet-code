package com.leetcode.cosminci._1200

import scala.collection.mutable

object _1140_StoneGameII:

  def stoneGameII(piles: Array[Int]): Int =
    val sSum = piles.scanRight(0)(_ + _).dropRight(1)

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, m: Int): Int = mem.getOrElseUpdate((i, m),
      if i + 2 * m >= sSum.length then sSum(i)
      else (1 to 2 * m).map(j => sSum(i) - dfs(i + j, j.max(m))).max
    )

    dfs(i = 0, m = 1)
