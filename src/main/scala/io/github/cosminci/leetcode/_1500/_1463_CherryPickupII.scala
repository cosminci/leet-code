package io.github.cosminci.leetcode._1500

import scala.collection.mutable

object _1463_CherryPickupII:
  def cherryPickup(grid: Array[Array[Int]]): Int =
    val mem = mutable.Map.empty[(Int, Int, Int), Int]
    def dfs(x: Int, y1: Int, y2: Int): Int = mem.getOrElseUpdate(
      (x, y1, y2), {
        if x == grid.length then 0
        else
          val currScore = Seq(y1, y2).distinct.map(y => grid(x)(y)).sum
          val y1next    = Seq(y1 - 1, y1, y1 + 1).filter(grid.head.isDefinedAt)
          val y2next    = Seq(y2 - 1, y2, y2 + 1).filter(grid.head.isDefinedAt)
          y1next.flatMap(y1 => y2next.map(y2 => dfs(x + 1, y1, y2))).max + currScore
      }
    )
    dfs(x = 0, y1 = 0, y2 = grid.head.length - 1)
