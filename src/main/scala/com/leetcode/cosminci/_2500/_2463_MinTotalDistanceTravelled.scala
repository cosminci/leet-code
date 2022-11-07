package com.leetcode.cosminci._2500

import scala.collection.mutable

object _2463_MinTotalDistanceTravelled:

  def minimumTotalDistance(robot: List[Int], factory: Array[Array[Int]]): Long =
    val robots    = robot.sorted.map(_.toLong)
    val factories = factory.map { case Array(pos, limit) => (pos.toLong, limit) }.sorted

    val mem = mutable.Map.empty[(Int, Int, Int), Long]
    def dfs(i: Int, j: Int, k: Int): Long = mem.getOrElseUpdate((i, j, k),
      if i == robots.length then 0
      else if j == factories.length then Long.MaxValue / 100
      else factories(j) match
        case (pos, limit) =>
          if limit <= k then dfs(i, j + 1, 0)
          else dfs(i, j + 1, 0).min(dfs(i + 1, j, k + 1) + (robots(i) - pos).abs)
    )

    dfs(i = 0, j = 0, k = 0)
