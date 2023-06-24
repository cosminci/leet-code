package com.leetcode.cosminci._2800

import scala.collection.mutable

object _2745_ConstructLongestNewString:

  def longestString(x: Int, y: Int, z: Int): Int =
    val mem = mutable.Map.empty[(Int, Int, Int, Int), Int]
    def dfs(prev: Int, x: Int, y: Int, z: Int): Int = mem.getOrElseUpdate((prev, x, y, z), {
      val useX = if x > 0 && prev != 0 then 2 + dfs(0, x - 1, y, z) else 0
      val useY = if y > 0 && prev <= 0 then 2 + dfs(1, x, y - 1, z) else 0
      val useZ = if z > 0 && prev != 0 then 2 + dfs(2, x, y, z - 1) else 0
      useX.max(useY).max(useZ)
    })
    dfs(prev = -1, x, y, z)
