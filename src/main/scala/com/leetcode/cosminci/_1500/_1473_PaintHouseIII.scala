package com.leetcode.cosminci._1500

import scala.collection.mutable

object _1473_PaintHouseIII:

  def minCost(houses: Array[Int], cost: Array[Array[Int]], m: Int, n: Int, target: Int): Int =
    val mem = mutable.Map.empty[(Int, Int, Int), Option[Int]]

    def dfs(i: Int, target: Int, prev: Int): Option[Int] = mem.getOrElseUpdate((i, target, prev),
      if i == m || target < 0 then Option.when(target == 0)(0)
      else if houses(i) != 0 then dfs(i + 1, target - Option.when(houses(i) != prev)(1).getOrElse(0), houses(i))
      else
        (1 to n).flatMap { color =>
          dfs(i + 1, target - Option.when(color != prev)(1).getOrElse(0), color)
            .map(_ + cost(i)(color - 1))
        }.minOption
    )

    dfs(i = 0, target, prev = -1).getOrElse(-1)
