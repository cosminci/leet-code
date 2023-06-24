package com.leetcode.cosminci._1000

import scala.collection.mutable

object _956_TallestBillboard:

  def tallestBillboard(rods: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, diff: Int): Int = mem.getOrElseUpdate((i, diff),
      if i == rods.length && diff == 0 then 0
      else if i == rods.length then Int.MinValue
      else dfs(i + 1, diff)
        .max(dfs(i + 1, diff - rods(i)))
        .max(dfs(i + 1, diff + rods(i)) + rods(i))
    )
    dfs(i = 0, diff = 0)
